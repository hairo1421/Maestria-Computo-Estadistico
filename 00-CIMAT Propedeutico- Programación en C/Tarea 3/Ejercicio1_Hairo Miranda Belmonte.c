/*Tarea 3
*Version 2
*Ejercicio 1
*Programa que suma de dos números binarios e imprima el resultado, así como la conversión
a decimal del resultado.
*/

#include <stdio.h>
#include<math.h>

 

int main(){
	int a,b, sum, dec, dec2;
	sum=0;
	dec=0;
	dec2=0;
	//ejercicio se hará con un binario de un digito dado que no hemos vistos arrays
	do  {
		printf("Ingresa el numero binario de un digito:");
		scanf("%i", &a);
	
 	}while(a!=1&&a!=0);
 	
 	do  {
		printf("Ingresa el segundo numero binario:");
		scanf("%i", &b);
	
 	}while(b!=1&&b!=0);
 	
 	if (a==0&&b==0){
 		sum=0;
	 } else if (a==1&&b==0){
 		sum=1;
 	} else if (a==0&&b==1){
 		sum=1;
	 }else {
	 	sum=10;
	 }
	 	printf("La suma de los binarios es:%i\n", sum);
	 	
	if (sum==0){
 		dec=pow(0,0);
 			printf("El binario a decimal es:%i%\n", dec);
	 } else if (sum==1){
 		dec=pow(1,0);
 			printf("El binario a decimal es:%i \n", dec);
 	} else if (sum==10){
 		dec=pow(2,0);
 		dec2=pow(2,1);
 			printf("El binario a decimal es:%i%i\n", dec, dec2);
	 }	 
	    


 	
 	return 0;
}


 

