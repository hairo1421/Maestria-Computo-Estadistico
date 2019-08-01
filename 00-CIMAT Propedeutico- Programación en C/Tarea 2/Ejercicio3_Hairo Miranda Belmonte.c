
/*Tarea 2
*Version 1
*Ejercicio 3
*Un programa que verificar las reglas de prioridad evaluación de expresiones a igualdad de
prioridades, en el caso de paréntesis. Comprobar que la evaluación se efectúa de izquierda a
derecha.
(explique procedimiento y resultado, dentro del programa)
*/

#include <stdio.h>
#include <math.h>
 

int main(){
	
	//formula general que saca las raices, el ejercicio es el presudocodigo de la tarea pasada en pseudocodigo
	int a,b,d, x1,x2, x3, x4;
	//no permite dar valores iguales a ceros o no sería función cuadratica
	do{
		printf("Ingresa el valor del coeficiente distinto de cero cuadrado:");
		scanf("%i", &a);
    }while (a==0);
		printf("Ingresa el valor del coeficiente del termino lineal:");
		scanf("%i", &b);
		printf("Ingresa el valor del coeficiente del termino independiente:");
		scanf("%i", &d);
	
	//Cuadratica
		//respetando prioridad
	d=sqrt(b*b-(4*a*b));
	x1=((b*-1)+(d))/(2*a);
	x2=((b*-1)-(d))/(2*a);
		printf("Respetando las reglas de prioridad, las raices son x1=%i, x2=%i\n", x1,x2);
	
	//sin respetar las prioridades
	d=sqrt(b*b-4*a*b);
	x3=b*-1+d/2*a;
	x4=b*-1-d/2*a;
		printf("Sin respetar las reglas de prioridad, las raices son x1=%i, x2=%i\n", x3,x4); 	
 	return 0;
}


 

