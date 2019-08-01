/*Tarea 3
*Version 1
*Ejercicio 10
*Solicitar tres números e indique el tipo de triángulo que forman (isósceles, equilátero,
escaleno). Comprobar que los números realmente formen un triángulo, sino emitir el error.
*/
#include<stdio.h>
void triangulo();
int main(){
	triangulo();
}

void triangulo(){
	float a,b,c;
	printf("introduce lado a de un posible triangulo:");
	scanf("%f",&a);
	printf("introduce lado b de un posible triangulo:");
	scanf("%f",&b);
	printf("introduce lado c de un posible triangulo:");
	scanf("%f",&c);
	
	if(a<b+c&&b<a+c&&c<a+b){
		printf("Es un triangulo\n");
		if (a==b&&a==c&&b==c){
			printf("Es equilatero");
		}else if (a==b||a==c||b==c){
			printf("Es isoceles");
		}else{
			printf("Es escaleneo");
		}
	}else{
		printf("No es un triangulo");
	}
	
}
