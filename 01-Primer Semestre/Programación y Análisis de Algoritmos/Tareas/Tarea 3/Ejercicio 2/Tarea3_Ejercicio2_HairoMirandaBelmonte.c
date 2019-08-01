/*Programación y Análisis de Algoritmos
**TAREA 3: PALABRAS CON NUMEROS TELEFONICOS.
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**01 DE SEPTIEMBRE DEL 2018*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
# define N 7


//prototipos
void permuta(int num[], int count, char resultado[], int n, char letras[10][5]);
void espacio(int num[], int n, char letras[10][5]);


int main()	{
	int i; //contador
	char letras[10][5] = {"", "", "abc", "def", "ghi", "jkl","mno", "prs", "tuv", "wxyz",};
	 //matriz de tamaño 10 por 5
	int num[N];//array donde se guardaran los números
	
	printf("Ingrese de uno en uno  7 digitos que no incluya 0 y 1 (dar enter por cada digito):\n");
	
	for(i=0;i<N;i++){
		do{//restricción, no saldra del loop hasta dar un número distinto de cero o uno
			scanf("\n%d", &num[i]);//introduce numero
			if(num[i]==0||num[i]==1){
				printf("Repite el digito, que no sea 0 y 1\n");
			}//end if
		}while(num[i]==0||num[i]==1);//end while 
	}// end for
	
	for(i=0;i<N;i++){
		printf("\n", num[i]);//imprime el numero que digita en forma hoorizontal
	}//end for
	int n = sizeof(num)/sizeof(num[0]);//tamaño bytes a n=7
	
	espacio(num, n,letras);//Funcion espacio, genera un vector
	//con elementos /0 los cuales se utilizaran para las combinaciones
	return 0;
}// end main

//función espacios, vector array para carácteres.
void espacio(int num[], int n, char letras[10][5]){
	
	char resultado[n+1]; //caracteres de 7+1 en este caso
	
	resultado[n] ='\0'; //llenar menos uno
	
	permuta(num, 0, resultado, n, letras);// función permuta
	//argumentos: vector númerico, parámetro inicial, vector carácteres, variable n (tamaño númerico a bytes)
	
}// end espacio

//funcion permutación: realiza combinaciones

//El proceso realiza lo siguiente. Comienza en cero y toma esa posición en el array del numero; redirecciona
//a la fila correspondiente a sus 3 letras de la matrriz con arrays; vuelve a permutar hasta que se haga 7 y sigue
//el for hasta terminar con las 3 letras.

void permuta(int num[], int count, char resultado[], int n, char letras[10][5]){

	int i;//contador
	if (count==n){ 	//contador que e pasamos en funciónespacio, su valor es cero
		printf("%s ", resultado);
		return ;// si el contador es igual a el tamaño n, entonces imprime el array de carateres vacios: resultado[n] ='\0';
	}// end if
	//las 10 a la 7 combinaciones posibles
	
	for (i=0; i<3; i++){//cada número redirecciona a una fila con tamaño de 3. Que son los elementos de la fila de letras 
		resultado[count]=letras[num[count]][i];//recorre la fila por columnas de uno por uno
		permuta(num, count+1, resultado	,n, letras);//para que realice todas las combinaciones con i=0,1,2, que son
		//las letras posibles de cada digito
	}//end for
		
} //end permmuta




