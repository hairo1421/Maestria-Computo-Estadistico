/*TAREA #3
**EJERCICIO #3
**VERSION 1.0
**HAIRO ULISES MIRANDA BELMONTE
**Introduce contraseña, si te equivocas tres veces,ERROR, de lo contrario FELICIDADES*/

#include<stdio.h>
#include<string.h>

int main (){
	int aux,i;
	aux=0;
	char pasw[8];
	//sólo acepta caracteres
	printf("introdusca contraseña no numerica:");
	fgets(pasw,8,stdin);
	char *c1=pasw;
	//inicializo el puntero
	char pasw2[8];
	printf("introdusca de nuevo la contraseña:");
	fgets(pasw2,8,stdin);
	char *c2=pasw2;
	//el ciclo que determina si es o no es igual. Se realiza 3 veces
	for(i=0;i<3;i++){
		if(*(c1+i)==*(c2+i)){
			aux=1;
			
		}else {
		printf("No se parecen las contraseñas:");
		fgets(pasw2,8,stdin);
		}
	}
	if(aux==1){
		printf("MUCHASFELICIDADES\n");
	}
	if(aux==0){
		printf("ERROR\n");
	}
}
