
/*Tarea 2
*Version 1
*Ejercicio 5
*5- Programa para comprobar los resultados de una confusión entre el operador "=" y el operador
"==".
(explique procedimiento y resultado, dentro del programa)
*/

#include <stdio.h>

 

int main(){
	int a1,a2, b;
	a1=0;
	a2=0;
		printf("se inicializan las variables a1=0 y a2=0\n");
		printf("Da un valor para una variable que sea b:");
		scanf("%i",&b);
	
	a1=b;
	a2==b;
		printf("Si realizamos lo siguente:\na1=b y a1==b\na1=b te da como resultado:%i.\na1==b te da resultado de:%i.\npor lo tanto = y == no son iguales", a1,a2);


 	return 0;
}
