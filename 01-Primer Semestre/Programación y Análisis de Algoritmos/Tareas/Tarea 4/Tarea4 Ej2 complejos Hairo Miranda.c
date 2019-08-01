/*Programación y Análisis de Algoritmos
**TAREA 4: OPERACIÓN CON NÚMEROS COMPLEJOS.
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**05 DE SEPTIEMBRE DEL 2018*/

#include<stdio.h>

typedef struct{ /* estructura*/
	
	int real;
	int imaginario;
	
}operaciones;

/* Prototipos de las funciones*/
void pedirNumeros(operaciones *complejo1,  operaciones *complejo2); /* pide números imaginarios por teclado*/
void suma(operaciones *complejo1, operaciones *complejo2); /* suma números complejos*/
void resta(operaciones *complejo1,operaciones *complejo2); /* resta números complejos*/
void multiplicacion(operaciones *complejo1,operaciones *complejo2); /* multiplica números */
void division(operaciones *complejo1,operaciones *complejo2); /* divide números complejos*/

int main()
{
	/*Declarando estructura*/
	operaciones complejo1, complejo2;
	
	/*Funciones*/
	pedirNumeros(&complejo1, &complejo2); /* pide números complejos*/
	suma(&complejo1, &complejo2); /* suma números complejos*/
	printf("\n");
	resta(&complejo1, &complejo2); /* resta números complejos*/
	printf("\n");
	multiplicacion(&complejo1, &complejo2); /* multiplicacion números complejos*/
	printf("\n");
	division(&complejo1, &complejo2); /* division números complejos*/
	
	return 0;
} // end main

/*
 * Función: pedirNumeros
 * ----------------------------
 *   Paso por referencia estructura. Pide números complejos
 *	
 *   operaciones *racional1: complejo que puede ser numero real o numero imaginario 
 *   operaciones *racional2: complejo que puede ser numero real o numero imaginario  
 *
 *   Función tipo void.
 */

void pedirNumeros(operaciones *complejo1, operaciones *complejo2 )
{ // pide número complejos por teclado
	
	/* Primer número imaginario*/
	printf("Introduzca el primero numero complejo:\n");
	printf("Parte real:\n");
	scanf("%d", &complejo1->real);
	printf("Parte imaginario:\n");
	scanf("%d", &complejo1->imaginario);
	
	/* Segundo número imaginario*/
	printf("Introduzca el segundo numero complejo:\n");
	printf("Parte real:\n");
	scanf("%d", &complejo2->real);
	printf("Parte imaginario:\n");
	scanf("%d", &complejo2->imaginario);
		
}// end pedirNumeros

/*
 * Función: suma
 * ----------------------------
 *   Paso por referencia estructura. Suma números complejos
 *	
 *   operaciones *racional1: complejo que puede ser numero real o numero imaginario 
 *   operaciones *racional2: complejo que puede ser numero real o numero imaginario  
 *
 *   Función tipo void.Imprime el resultado de la suma de números complejos
 */

void suma(operaciones *complejo1,operaciones *complejo2)
{ /* suma números imaginarios*/
		
	printf("La suma de los numeros complejos es: (%d %di)\n", complejo1->real + complejo2->real, 
	complejo1->imaginario + complejo2->imaginario); /* suma partes reales y partes complejas*/
	
}// end suma

/*
 * Función: resta
 * ----------------------------
 *   Paso por referencia estructura. Resta números complejos
 *	
 *   operaciones *racional1: complejo que puede ser numero real o numero imaginario 
 *   operaciones *racional2: complejo que puede ser numero real o numero imaginario  
 *
 *   Función tipo void.Imprime el resultado de la resta de números complejos
 */

void resta(operaciones *complejo1,operaciones *complejo2)
{ /*resta número imaginarios*/
		
	printf("La resta de los numeros complejos es: (%d %di)\n", complejo1->real - complejo2->real, 
	complejo1->imaginario - complejo2->imaginario); /* resta partes reales y partes complejas*/
	
}// end resta

/*
 * Función: multiplica
 * ----------------------------
 *   Paso por referencia estructura. Multiplica números complejos
 *	
 *   operaciones *racional1: complejo que puede ser numero real o numero imaginario 
 *   operaciones *racional2: complejo que puede ser numero real o numero imaginario  
 *
 *   Función tipo void.Imprime el resultado de la multiplicaión de números complejos
 */

void multiplicacion(operaciones *complejo1,operaciones *complejo2)
{ /* Multiplicación de números complejos*/
	
	/* variables auxiliares*/
	int real= 0;
	int imaginario = 0;
	
	/*Multiplicación por propiedad distributiva*/
	real = complejo1->real*complejo2->real;
	imaginario = complejo1->real*complejo2->imaginario;
	imaginario += complejo1->imaginario*complejo2->real;
	real += (complejo1->imaginario*complejo2->imaginario)*(-1); /* i al cuadrado es igual a -1*/
	
	printf("La multiplicacion de los numeros complejos es: (%d %di)\n", real, imaginario);
	
}// end multiplicación

/*
 * Función: division
 * ----------------------------
 *   Paso por referencia estructura. Suma números complejos
 *	
 *   operaciones *racional1: complejo que puede ser numero real o numero imaginario 
 *   operaciones *racional2: complejo que puede ser numero real o numero imaginario  
 *
 *   Función tipo void.Imprime el resultado de la división de números complejos
 */

void division(operaciones *complejo1,operaciones *complejo2)
{ /* División de números complejos*/
	
	/* variables auxiliares*/
	int realNum = 0;
	int realDen = 0;
	int imaginarioNum = 0;
	int imaginarioDen = 0;
	
	/* numerador Primer numero compleho por numerador del conjugado*/
	realNum = complejo1->real*complejo2->real;
	imaginarioNum = complejo1->real*((-1)*complejo2->imaginario);
	imaginarioNum += complejo1->imaginario*complejo2->real;
	realNum += (complejo1->imaginario*((-1)*complejo2->imaginario))*(-1); // i al cuadrado es igual a -1
	/* denominador Segundo numero compleho por denominador del conjugado*/
	realDen = complejo2->real*complejo2->real;
	imaginarioDen = complejo2->real*((-1)*complejo2->imaginario);
	imaginarioDen += complejo2->imaginario*complejo2->real;
	realDen += (complejo2->imaginario*((-1)*complejo2->imaginario))*(-1); // i al cuadrado es igual a -1
	realDen = realDen+imaginarioDen; //por el conjugado, ambos quedan como reales
	
	printf("La division de los numeros complejos es: ((%d %di)/%d)\n", realNum, imaginarioNum, realDen);
	
}// end division
