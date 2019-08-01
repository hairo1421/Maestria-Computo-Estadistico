/*Programación y Análisis de Algoritmos
**TAREA 5: SIMULACION COLAS CON ALEATORIOS DEL 1 AL 3
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**20 DE SEPTIEMBRE DEL 2018*/

#include<stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
typedef struct { /*Declarando estructura con campo int*/
int num;
} QueueData;

typedef struct node { /*Estructura que genera la cola (tipo linked list)*/
QueueData data;
struct node *next; 
} Node, *NodePtr;

typedef struct queueType { /*Estructura cola que apuntan a inicio al final*/
NodePtr head, tail;
} QueueType, *Queue;

/*
 * Función: initQueue
 * ----------------------------
 *   Genera Cola dando espacio de memoria de tamaño de estructura
 *
 *	 Input: Nada
 *   Return: Cola 
 *
 *   Función tipo Queue.
 */
Queue initQueue()
{
	Queue qp = (Queue) malloc(sizeof(QueueType));
	qp -> head = NULL; /*Apunta al inicio*/
	qp -> tail = NULL; /*Apunta al final*/
	return qp;
} //end initQueue

/*
 * Función: empty
 * ----------------------------
 *   Rectifica que se asignara correctamente el espacio de 
 *	 memoria a la cola
 *
 *	 Input: Cola
 *   Return: Expresión lógica 
 *
 *   Función tipo int.
 */
int empty(Queue Q)
{
	return (Q -> head == NULL);
} //end empty
/*
 * Función: enqueue
 * ----------------------------
 *   Crea Nodo dentro de cola (lista enlazada)
 *
 *	 Input: Estructura cola, Estructura int 
 *   Return: Nada 
 *
 *   Función tipo Void.
 */
void enqueue(Queue Q, QueueData d) 
{
	int i = 0;	
	NodePtr np = (NodePtr) malloc(sizeof(Node));
	np -> data = d;
	np -> next = NULL;
	if (empty(Q)) { /*Cola vacia, inicio y final de cola apuntan al nulo (nodo generado)*/
		Q -> head = np;
		Q -> tail = np;
	} else {
		Q -> tail -> next = np;
		Q -> tail = np;
	}
} //end enqueue

/*
 * Función: dequeue
 * ----------------------------
 *   Retira Nodo dentro de cola (lista enlazada)
 *
 *	 Input: Estructura cola 
 *   Return: nodo eliminado (valor int)
 *
 *   Función tipo QueueData.
 */

QueueData dequeue(Queue Q)
 {
	if (empty(Q)) {
		printf("\nCola vacia\n");
		exit(1);/*Error en la ejecución*/
	}
	QueueData hold = Q -> head -> data; /*Guarda valor numerico de nodo para imprimir*/
	NodePtr temp = Q -> head;
	Q -> head = Q -> head -> next; /*Apunta donde apunta el nodo a eliminar*/
	if (Q -> head == NULL) Q -> tail = NULL;/*si inicio está vacio, apunta final a nulo*/
		free(temp);
	return hold; /*nodo eliminado (valor int)*/
} //end dequeue

/*
 * Función: arrive
 * ----------------------------
 *   Programa tiempo de llegada de cada cliente en el tiempo
 *
 *	 Input: Nada
 *   Return: array
 *
 *   Función tipo int puntero.
 */

int *arrive()
{	
	srand(time(NULL)); /*Fija semilla*/
	int t = 0; int clientes = 0; int i =0 ; /*variables auxiliares*/
	 int * tiempoLlegada  =( int *) calloc ( 720, sizeof ( int *) );

	/*Programa minutos de llegada a cada cliente*/
	for( t = 1; t <= 720; t++){	
		clientes = (clientes + (1 + ( rand() % 3 )));/*dato aleatorio*/
		if(clientes <= 720){
			tiempoLlegada[t] = clientes; /*Acumula lanzamientos para el tiempo de llegada*/
		}// end if
	}// end for
	
	return tiempoLlegada;/*Regresa tiempo de llegada*/
	/*No libero espacio de memoria, se seguitra utilizando array*/
}// end arrive
/*
 * Función: service
 * ----------------------------
 *   Programa tiempo de llegada de cada cliente en el tiempo
 *
 *	 Input: Nada
 *   Return: array
 *
 *   Función tipo int puntero.
 */


int *service()
{
    int i = 0; int contar = 0; int servicios = 0; int t = 0; /*variables auxiliares*/
    int *p; /*Puntero, almacena array*/
    p = arrive();/*array con llegadas de clientes*/
	srand(time(NULL));/*Fijamos semilla*/
				
    for(i=0; i <1000 ; i++){  /*No se sabe el size, se filtra información*/
   		if(p[i]!=0 && p[i] > 0 && p[i] < 800){ /*No me puede dar un tiempo mayor a 720*/
   			contar= contar+1;
	 	}// end if	
	}// end for
   
   /*Se crea aquí el array dado a que no se sabía el espacio de memoria que se utilizaríá*/
    int * tiempoAtencion =( int *) calloc ( contar, sizeof ( int *) );
	
	for( t = 1; t <= contar; t++){/*Despachar  tantos clientes cuenten*/
		servicios = 0;
		servicios = (1 + ( rand() % 3 ));/*cuanto duran en atender a los clientes que van a llegar*/
		tiempoAtencion[t] = servicios; /*Debe acumularse*/
	}// end for

return tiempoAtencion;
/*No libero espacio de memoria, se seguitra utilizando array*/
}/*end service*/
/*
 * Función: simulation
 * ----------------------------
 *   Simula entradas y salidas
 *
 *	 Input: Nada
 *   Return: Nada
 *
 *   Función tipo in..
 */

int simulation()
{
	QueueData temp; /*Declarando estructura con campo int*/
	Queue Q = initQueue();/*Genera espacio de memoria a la cola*/
	temp.num = 0; int t = 0; int acumulado = 0; int i = 0; int contar = 0;/*variables auxiliares*/
	int countNodo = 0;int  max[900];int j= 0;int aux =0;
	
	static	int *p=0; static	int *q=0;/*Variables que almacenan los arrays*/	
	
	p = arrive();/*Llegada de clientes*/
	q = service();/*Salida de clientes*/
	
	for(i=0; i <1000 ; i++){  /*No se sabe el size, filtro información*/
   		if(p[i]!=0 && p[i] > 0 && p[i] < 800){ /*No me puede dar un tiempo mayor a 720*/
   			contar= contar+1; /*Numero de clientes que se recibieron en los 720 minutos*/
	 	}// end if	
  	}// end for 
	
	/*Simula para los clientes */
	for (i =1; i< contar; i++){ /*i no lo pongo a inicio del array con el fin de poder tomar sus ceros al inicio*/
		for (t = 1; t <=720; t++){ /*Si el tiempo es igual a la llegada estimada del cliente entonces crea nodo*/
			if(t== p[i]){ /*Entrada*/
				temp.num = i; 
				enqueue(Q, temp);
				printf("Cliente %d: Entrada minuto %d", temp.num, t);/*imprime numero de cliente y minuto de entrada*/
				printf("\n");
				printf("Cliente %d: Cajero lo atiende en  %d minutos", temp.num, q[i]);/*tiempo de atención*/
				printf("\n");
			
			if((p[i-1]+q[i-1]) <= p[i]){ /*Si el clinte no espera al ser atendido encotnces */
				printf("Cliente %d: Espero %d minutos", temp.num, 0); /*Tiempo de espera igual a cero*/
				printf("\n");
				printf("Cliente %d: Atendido en el minuto %d",temp.num,p[i]);  /*Minuto en el que fue atendido*/
				printf("\n");			
				printf("Cliente %d: Salida %d", dequeue(Q).num, (p[i]+q[i])); /*Minuto en el que salio de la tienda*/
				printf("\n");
			
			} else  {  /*Si el clinte  espera al ser atendido encotnces */
				printf("Cliente %d: Espero %d", temp.num, (p[i-1]+q[i-1])-p[i]);  /*Tiempo de espera igual a cero*/
				printf("\n");
			 
			
				printf("Cliente %d: Atendido en el minuto %d",temp.num,p[i-1]+q[i-1]);  /*Minuto en el que fue atendido*/
				printf("\n");
				printf("Cliente %d: Salida %d", dequeue(Q).num, p[i-1]+q[i-1]+q[i]); /*Minuto en el que salio de la tienda*/
				printf("\n");	
			}// end else	
				max[i]=fabs((p[i-1]+q[i-1]-p[i]));/*espera más larga de un cliente*/
			}// end if
		}// end for
			printf("\n");
	}// end for
	for (i = 1; i < contar; ++i) { /*contesta la pregunta del tiempo más largo de espera*/
        for (j = i + 1; j < contar; ++j){
            if (max[i] < max[j])  {
                aux =  max[i];
                max[i] = max[j];
                max[j] = aux;
            }//end if
        }// end for
    }// end for
    
	printf("Pregunta\n ");				
	printf("Cual es la espera mas larga que un cliente experimento: %d ",max[1]);
			
}//end simulation


int main () {

 simulation();
   
   return 0;
}// end main






