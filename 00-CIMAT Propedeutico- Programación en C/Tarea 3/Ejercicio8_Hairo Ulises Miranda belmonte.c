/*Tarea 3
*Version 1
*Ejercicio 8
*Programa que realice la 4 operación básica con número romanos, se debe ingresar los dos
numero romanos y tipo de operación a realizar, finalmente se imprimirá el resultado en número
decimal.
*/
#include<stdio.h>
#include <string.h>
//numeros romanos solamennte llegan al 1000
void romano();


int main (){

	romano();

}

void romano(){

char rom[50];
int dec[50];
int n, i,j, aux,sum;
aux=0;
sum=0;
printf("introduce el romano:");
scanf("%s", rom);
n=strlen(rom);//toma longitud de la cadena de caracteres
	for(i=0;i<n;i++){
		if(rom[i]=='I'||rom[i]=='i'){
		dec[i]=1;
		}
		if(rom[i]=='V'||rom[i]=='v'){
		dec[i]=5;
		}
		if(rom[i]=='X'||rom[i]=='x'){
		dec[i]=10;
		}
		if(rom[i]=='L'||rom[i]=='l'){
		dec[i]=50;
		}
		if(rom[i]=='C'||rom[i]=='c'){
		dec[i]=100;
		}
		if(rom[i]=='D'||rom[i]=='d'){
		dec[i]=500;
		}
		if(rom[i]=='M'||rom[i]=='m'){
		dec[i]=1000;
		}
		
	}


	
		for(j=0;j<n;j++){
	if(j==n-1){
		aux+=dec[j];	
	}else if (dec[j]>=dec[j+1]){
	
	aux+=dec[j];}
	else {
	
	aux-=dec[j];}	
		
	}
	
	
	
	
	printf("%i\n",aux);
//segundo romano	
char rom2[50];
int dec2[50];
int n2,aux2,w,y,h;
aux2=0;
printf("introduce el otro romano:");
scanf("%s", rom2);
n2=strlen(rom2);//toma longitud de la cadena de caracteres
	for(w=0;w<n2;w++){
		if(rom2[w]=='I'||rom2[w]=='i'){
		dec2[w]=1;
		}
		if(rom2[w]=='V'||rom2[w]=='v'){
		dec2[w]=5;
		}
		if(rom2[w]=='X'||rom2[w]=='x'){
		dec2[w]=10;
		}
		if(rom2[w]=='L'||rom2[w]=='l'){
		dec2[w]=50;
		}
		if(rom2[w]=='C'||rom2[w]=='c'){
		dec2[w]=100;
		}
		if(rom2[w]=='D'||rom2[w]=='d'){
		dec2[w]=500;
		}
		if(rom2[w]=='M'||rom2[w]=='m'){
		dec2[w]=1000;
		}
		
	}

	for(y=0;y<n2;y++){
	if(y==n2-1){
		aux2+=dec2[y];	
	}else if (dec2[y]>=dec2[y+1]){
	
	aux2+=dec2[y];}
	else {
	
	aux2-=dec2[y];}	
		
	}
	printf("%i\n",aux2);


	do{
		printf("selecciones tipo de operación:\n1=suma\n2=resta\n3=multiplicacion\n4=division\n");
		scanf("%i",&h);
	}while(h<1||h>4);
	
	if(h==1){
		sum=aux+aux2;
		printf("la suma de los romanos es: %i\n",sum);
	} else
	if(h==2){
		sum=aux-aux2;
		printf("la resta de los romanos es: %i\n",sum);
	} else
	if(h==3){
		sum=aux*aux2;
		printf("la multiplicacion de los romanos es: %i\n",sum);
	} else
	if(h==4){
		sum=aux/aux2;
		printf("la division de los romanos es: %i\n",sum);
	} 
	
	


	
}

	
	
