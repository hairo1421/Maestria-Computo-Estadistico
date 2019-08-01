/*Tarea 3
*Version 1
*Ejercicio 13
*Calcular el salario semanal de los empleados, cada hora extra se deberá pagar 7 Dólar, si las
horas extras no superen las 20 horas. Por cada hora por encima de 20 se debe pagar 14 dólar.
*/
#include<stdio.h>
void salario();
int main(){

salario();
}

void salario(){
	int i, j, s1, s2, income, aux, aux2,n,m;
	s1=0;
	s2=0;
	income=0;
	aux=0;
	aux2=0;
	
	do{
		printf("Dame las horas extras que trabajaste :");
		scanf("%i", &n);	
	}while (n<0);
	
			
if(n>1||n<=20){
	s1=7*n;
		aux+=s1;	
}else if(n>20){
	s2=14*n;
}
	
	
	income=aux+aux2;
	printf("El salario que gana por hora extra es de %i dolares",income);
}
