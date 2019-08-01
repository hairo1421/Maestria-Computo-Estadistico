/*Tarea 3
*Version 1
*Ejercicio 4
*Comprobar si un determinado número es Armstrong.
*/
#include<stdio.h>
#include<math.h>
void armstrong();
int main(){
	armstrong();
}
void armstrong(){
	int n,m,aux, aux2,sum,aux4;
	float aux3;
	printf("Digita un numero entero positivo:");
	scanf("%i",&n);
	printf("cunatos digitos tiene tu numero:");
	scanf("%i",&m);
	aux4=n;
		while (n!=0){
			aux=(n%10);
			aux2=pow(aux,m);
			sum+=aux2;
			n=n/10;
			
		}
	aux3=aux4/sum;
	if(aux3==1){
			printf("El resultado es:%i, por lo tanto es un numero armstrong",sum);
	}else{
			printf("El resultado es:%i, por lo tanto no es un numero armstrong",sum);	
	}
	
	
	
}
