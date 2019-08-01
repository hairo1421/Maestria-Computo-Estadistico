/*Programación y Análisis de Algoritmos
**TAREA 4: OPERACIÓN CON RACIONALES
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**05 DE SEPTIEMBRE DEL 2018*/

#include<stdio.h>

typedef struct{ /*estructura*/
	
	int numerador;
	int denominador;
	
}operaciones;

/* Prototipos de las funciones*/

void simplificar(int a, int b); /* simplifica resultado de operaciones entre fracciones*/
void pedirNumeros(operaciones *racional1,  operaciones *racional2); /* recibe raconal por teclado*/
void suma(operaciones *racional1,  operaciones *racional2); /* suma de racionales*/
void resta(operaciones *racional1,  operaciones *racional2); /* resta de racionales*/
void multiplicacion(operaciones *racional1,  operaciones *racional2); /* multiplicación de racionales*/
void division(operaciones *racional1,  operaciones *racional2); /* división de racionales*/


int main()
{	
	/*Declarando struct*/
	operaciones racional1, racional2;
	
	/*Funciones*/
	pedirNumeros(&racional1, &racional2); /* pide números racionales*/
	
	suma(&racional1, &racional2); /* suma números racionales*/
	printf("\n");
	resta(&racional1, &racional2); /* resta números racionales*/
	printf("\n");
	multiplicacion(&racional1, &racional2); /*multiplicacion números racionales*/
	printf("\n");
	division(&racional1, &racional2); /*division números racionales*/
	
	return 0;
} //end main

/*
 * Función: pedirNumero
 * ----------------------------
 *   Paso por referencia estructura. Pide números racionales
 *	
 *   operaciones *racional1: racional que puede se numerados o denominador 
 *   operaciones *racional2: racional que puede se numerados o denominador 
 *
 *   Función tipo void.
 */
 
void pedirNumeros(operaciones *racional1, operaciones *racional2 )
{ /* pide número racional*/
	
	/* Primer numero racional*/
	printf("Introduzca el primero numero racional:\n");
	printf("Numerador:\n");
	scanf("%d", &racional1->numerador);
	printf("Denominador:\n");
	scanf("%d", &racional1->denominador);
	
	/* Segundo numero racional*/	
	printf("Introduzca el segundo numero racional:\n");
	printf("Numerador:\n");
	scanf("%d", &racional2->numerador);
	printf("Denominador:\n");
	scanf("%d", &racional2->denominador);
		
}// end pedirNumeros

/*
 * Función: suma
 * ----------------------------
 *   Paso por referencia estructura. Suma números racionales.
 *	
 *   operaciones *racional1: racional que puede se numerados o denominador 
 *   operaciones *racional2: racional que puede se numerados o denominador 
 *
 *   Función tipo void. Imprime el resultado de la suma de números racionales
 */
 
void suma(operaciones *racional1,operaciones *racional2)
{ /* suma números racionales*/
		
	/* variables auxiliares*/
	int num = 0;
	int den = 0;
		
	if(racional1->denominador == racional2->denominador){ /* Caso 1: mismo denominador*/
		num = racional1->numerador+racional2->numerador;
		den = racional1->denominador;
		printf("La suma de los numeros racionales es: %d/%d\n", num, den);
	} else { /* Caso 2: denominadores diferentes*/
		num = racional1->numerador*racional2->denominador + racional1->denominador*racional2->numerador,
		den = racional1->denominador*racional2->denominador;
		printf("La suma de los numeros racionales es: %d/%d\n", num, den);
	} //end else
		
	simplificar(num, den); /* simplifica resultado */
		
}//end suma

/*
 * Función: resta
 * ----------------------------
 *   Paso por referencia estructura. resta números racionales.
 *	
 *   operaciones *racional1: racional que puede se numerados o denominador 
 *   operaciones *racional2: racional que puede se numerados o denominador 
 *
 *   Función tipo void. Imprime el resultado de la resta de números racionales.
 */
void resta(operaciones *racional1,operaciones *racional2)
{ /* resta números racionales*/

	/* variables auxiliares*/
	int num = 0;
	int den = 0;
	
	if(racional1->denominador == racional2->denominador){ /* Caso 1: mismo denominador*/
		num = racional1->numerador - racional2->numerador;
		den = racional1->denominador;
		printf("La resta de los numeros racionales es: %d/%d\n", num, den);
	} else { /* Caso 2: denominadores diferentes*/
		num = racional1->numerador*racional2->denominador - racional1->denominador*racional2->numerador,
		den = racional1->denominador*racional2->denominador;
		printf("La resta de los numeros racionales es: %d/%d\n", num, den);
	}// end else	
		
	simplificar(num, den);  /* simplifica resultado*/
		
	
}//end resta

/*
 * Función: multiplicación
 * ----------------------------
 *   Paso por referencia estructura. Multiplica números racionales.
 *	
 *   operaciones *racional1: racional que puede se numerados o denominador 
 *   operaciones *racional2: racional que puede se numerados o denominador 
 *
 *   Función tipo void. Imprime el resultado de la multiplicación de nímeros racionales.
 */
 
void multiplicacion(operaciones *racional1,operaciones *racional2)
{  /* multiplicación números racionales*/
	
	/* variables auxiliares*/
	int num = 0;
	int den = 0;
	
	num = racional1->numerador * racional2->numerador; /* numerodos por numerador*/
	den = racional1->denominador * racional2->denominador; /* denominador por denominador*/
	
	printf("La multiplicacion de los numeros racionales es: %d/%d\n", num, den);
	
	simplificar(num, den);  /* simplifica resultado */
	
}// end multiplicación

/*
 * Función: división
 * ----------------------------
 *   Paso por referencia estructura. Divide números racionales.
 *	
 *   operaciones *racional1: racional que puede se numerados o denominador 
 *   operaciones *racional2: racional que puede se numerados o denominador 
 *
 *   Función tipo void. Imprime el resultado de la divide de nímeros racionales.
 */
 
void division(operaciones *racional1,operaciones *racional2)
{  /* división números racionales*/
	
	/* variables auxiliares*/
	int num = 0;
	int den = 0;
	
	/* producto cruzado*/
	num = racional1->numerador * racional2->denominador; 
	den = racional1->denominador * racional2->numerador;
	
	printf("La division de los numeros racionales es: %d/%d\n", num, den);
	
	simplificar(num, den); /* producto cruzado*/
	
}// end division 

/*
 * Función: simplificar
 * ----------------------------
 *   Simplifica los resultados de las operaciones de los racionales.
 *	
 *   a: numerador de un número racional 
 *   b: denominador de un número racional 
 *
 *   Función tipo void. Imprime el resultado de las operaciones simplificada de los racionales.
 */
 
void simplificar(int a, int b)
{ /* Funcion que simplifica los resultados de las operaciones*/
	  
	/* variable auxiliar*/
	int fraccion = 0;
	 
	if (abs(a) > abs(b) && abs(a) % abs(b) == 0 && a!=0){ /* Caso 1: numerador mayor a denominador*/
	 	fraccion = a/b;
	 	printf("\nLa fraccion simplificada es:%d\n", fraccion);  
	} else if(abs(a) < abs(b)  && a!=0){ /* Caso 2: numerador menor a denomindador*/
	 	b = b/a;
	 	a= a/b;
	 	printf("\nLa fraccion simplificada es:%d/%d\n", a, b);
	} else if(abs(a) == abs(b) && abs(a) % abs(b) == 0){ /* Caso 3: numerador igual a denomindador*/
	 	fraccion= a/b;
	 	printf("\nLa fraccion simplificada es:%d\n", fraccion);
	} else if (a==0){ /* Caso 4: numerador es cero*/
	 	printf ("\nIndeterminado\n");
	} else if (b==0){ /* Caso 5: denominador es cero*/
	 	fraccion= 0;
	 	printf("\nLa fraccion simplificada es:%d\n", fraccion);
	} // end if	 
	
} // end simplificar
