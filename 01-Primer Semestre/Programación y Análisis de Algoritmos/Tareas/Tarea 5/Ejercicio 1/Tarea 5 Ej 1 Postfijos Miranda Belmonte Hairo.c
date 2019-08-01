/*Programación y Análisis de Algoritmos
**TAREA 5: OPERACION POSTFIJA
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**19 DE SEPTIEMBRE DEL 2018*/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>

/*Estructuras*/
typedef struct { /* Estrcturas elementos nodos*/
char ch;
int num;
} StackData;

/*Lista enlazada (nodos)*/
typedef struct node { /*Generador de Nodos de la lista de la pila*/
StackData data;
struct node *next;
} Node, *NodePtr;

/*Pila (realizada lista enlazada)*/
typedef struct stackType { /*Estructura pila, referenciada a una lista enlazada*/
NodePtr top;
} StackType, *Stack;

/*Prototipos*/

	/*Tipo int*/
int esVacio(Stack S ); /*Rectifica si a la pila se le asigno espacio de memoria*/
int evaluarExpresionPostfija(char[]);/*Evalua expresión postfija*/
int calcular(int a, int b, char d); ;/*Da resultado de expresión postfija*/
	/*Tipo void*/
void push(Stack S, StackData d);/*Genera nodos e introduce al inicio de la pila (en lista enlazada)*/
	/*Tipo otros*/
StackData pop(Stack S);/*Elimina nodos del inicio de la pila*/
Stack initStack(); /*Le da espacio de memoria a la pila*/


int main() 
{
	char post[50]; /*guardan exp. postfija*/
	printf("\nEl resultado es: %d\n", evaluarExpresionPostfija(post) );/*Imprime resultado  expresión postfija*/

} //end main

/*
 * Función: initStack
 * ----------------------------
 *   Asigna espacio de memoria a la estructura de tipo pila.
 *	 Da valor nulo a la lista enlazada (no hay nodos).
 *   
 *	 Input: Nada
 *   Return: Pila con espacio de memora, y lista que apunta a Null.
 *
 *   Función tipo Stack.
 */
Stack initStack()
{ 
	Stack sp = (Stack) malloc(sizeof(StackType)); /*Asigna espacio de memoria*/
	sp -> top = NULL;/*Primer nodo nulo*/
	return sp;/*Regresa pila*/
} //end initStack

/*
 * Función: esVacio
 * ----------------------------
 *   Revisa si la asignación de memoria a la pila fue corecta,
 *	 revisando que el inicio de la lista enlazada sea null .
 *   
 *	 Input: Pila
 *   Return:Expresión de si está o no vacia la pila
 *
 *   Función tipo Stack.
 */
int esVacio(Stack S )
{
 	return (S -> top == NULL);
 } //end esVacio

/*
 * Función: push
 * ----------------------------
 *   Genera nodo dentro de la lista con la que se realiza la pila
 *
 *	 Input: Pila, estructura que contiene variable tipo int
 *   Return: Nada
 *
 *   Función tipo void.
 */
void push(Stack S, StackData d) 
{
	NodePtr np = (NodePtr) malloc(sizeof(Node));/*Crea nodo, asigna memora*/
	np -> data = d; /*apunta valor nodo a valor a asignar*/
	np -> next = S -> top; /*puntero de nuevo nodo apunta a donde apunta  inicio de  pila*/
	S -> top = np; /*asigna nuevo nodo a la cabeza de la pila */
} //end push

/*
 * Función: pop
 * ----------------------------
 *   Elimida nodo de inicio de pila
 *
 *	 Input: Pila
 *   Return: Valor numerico del nodo eliminado
 *
 *   Función tipo StackData.
 */
StackData pop(Stack S)
{
	if (esVacio(S)) { /*Revisa si pila no está vacia*/
		printf("\nLa pila se encuentra vacia\n");
		exit(1); /*Indica proceso sin éxito*/
	}
	StackData hold = S -> top -> data; /*Retiene valor núm del nodo a inicio de pila*/
	NodePtr temp = S -> top; /*Retiene nodo a inicio de pila*/
	S -> top = S -> top -> next;/*Nodo  inicio de pila, apunta a donde apunta él mismo*/
	free(temp);/*Librea memoria del nodo eliminado*/
	return hold; 
}// end pop

/*
 * Función: calcular
 * ----------------------------
 *   Realiza operación correspondiente de la expresión postfija
 *
 *	 Input: Los primeros dos elementos de la Pila, carácter con el signo de la operacióna realizar
 *   Return: Resultado de la operación postfija
 *
 *   Función tipo int.
 */
int calcular(int a, int b, char d)
 {
	int c = 0;/*variable resultado*/
	if (d == '+'){
		c = a + b;/*suma*/
	} else if (d == '-'){
		c = a - b;/*resta*/
	} else if (d == '*') {
		c = a * b;/*multiplica*/
	} else if (d == '^'){
		c = pow(a, b);/*expenencial*/
	} else {
		c = a / b;
	}
	return (c);/*operación correspondiente a dos valores de pila*/
} //end calcular
/*
 * Función: evaluarExpresionPosfija
 * ----------------------------
 *   Evalua expresión posfija
 *
 *	 Input: array de carácteres
 *   Return: Resultado de la evaluación posfija
 *
 *   Función tipo StackData.
 */
int evaluarExpresionPostfija(char post[]) 
{
	StackData temp;
	/*variables auxiliares*/	
	int a = 0; int b = 0; int i = 0; int h = 0; char ch; char d;
	Stack S = initStack();/*Asigno espacio de memora a la pila*/

	printf("Escreibe una expresion postfija y presione Enter:\n");
	ch = getchar();/*recibe un carácter de un strem*/

	while (ch != '\n') { /*coloca carácteres en array*/
		post[h++] = ch;
		ch = getchar();
	} //end while

	while (!esVacio(S)){ /*Revisa que pila no esté vacia*/
		post[h++] = pop(S).ch;/*asigna carácter de nodo a inicio de arras*/
	} 

	for (i = 0; i < h; i++) { /*evualuando expresión postfija*/
		if (isdigit(post[i])) { /*Revisa si expresión es digito*/
			temp.num = post[i] - '0'; /*No toma encuenta el valor 0*/
			push(S, temp);/*Crea nodo al inicio de pila*/
		} else { /*Toma primeros dos elementos de pila*/
			b = pop(S).num; /*Primer elemento en salir, segundo operando*/
			a = pop(S).num;/*Segundo elemento en salir, primer operando*/
			d = post[i]; /*Guarda perador*/
			temp.num = calcular(a, b, d) ; /*resuelve operación */
			push(S, temp);/*intoduce operacio, nuevo nodo a inicio de pila*/
		}
	}
	return pop(S).num;/*Regresa valor numerico de nodo eliminado*/
} //end evaluarExpresionPostfija









