
/*Tarea 3
*Version 2
*Ejercicio 2
*4- Programa para convertir octal a binario.
*/

#include <stdio.h>

 

int main(){
	int n;
	
	do{
		printf("Selecciones un octal; es decir, de 0 a 7:   ");
		scanf("%i", &n);
	}while(n<=0||n>=7);
	
	switch(n){
		case 0:printf("El numero binario del octal %i seleccionado es: 000",n); break;
		case 1:printf("El numero binario del octal %i seleccionado es: 001",n); break;
		case 2:printf("El numero binario del octal %i seleccionado es: 010",n); break;
		case 3:printf("El numero binario del octal %i seleccionado es: 011",n); break;
		case 4:printf("El numero binario del octal %i seleccionado es: 100",n); break;
		case 5:printf("El numero binario del octal %i seleccionado es: 101",n); break;
		case 6:printf("El numero binario del octal %i seleccionado es: 110",n); break;
		case 7:printf("El numero binario del octal %i seleccionado es: 111",n); break;
		
	}
	
 	return 0;
}


 

