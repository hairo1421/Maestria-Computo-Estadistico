/*Tarea 3
*Version 1
*Ejercicio 7.1
*Solicitar un día de la semana y que nos diga si es un día laboral o no (semana laborar de
lunes-viernes). Usa 2 diferentes estructuras secuenciales..
*/
#include<stdio.h>

void laboral();


int main (){

	laboral();

}

void laboral(){
	int n;
	do{
		printf("Dias a la semana:\n");
		printf("Lunes=1\n");
		printf("Martes=2\n");
		printf("Miercoles=3\n");
		printf("Jueves=4\n");
		printf("Viernes=5\n");
		printf("Sabado=6\n");
		printf("Domingo=7\n");
		printf("Introduce un dia a la semana:");

		scanf("%i", &n);
	}while(n<1||n>7);
	
	switch(n){
		case 1:printf("Es dia laboral");break;
		case 2:printf("Es dia laboral");break;
		case 3:printf("Es dia laboral");break;
		case 4:printf("Es dia laboral");break;
		case 5:printf("Es dia laboral");break;
		case 6:printf("Es dia no laboral");break;
		case 7:printf("Es dia no laboral");break;
		}
		
	   
	}
