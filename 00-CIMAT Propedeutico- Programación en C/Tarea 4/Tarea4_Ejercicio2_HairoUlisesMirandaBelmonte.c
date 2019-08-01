/*TAREA #3
**EJERCICIO #2
**VERSION 1.0
**HAIRO ULISES MIRANDA BELMONTE
**Normalizar un vector*/
#include<stdio.h>
#include<math.h>

int main(){
	int n,i,j,k,w;
	float sum,root;
	//bubble method; almacenamiento de valores
	sum=0;
	root=0; 
   
	printf("Introduce longitud del vector:");
	scanf("%i", &n);
	//declarar arrays putero que apunta a memoria de otro
	float v[n];
	float *v2;
	v2=v;
	//introduciendo valores  al primer vector
	for(i=0;i<n;i++){
		printf("Introduce elementos vector:");
	scanf("%f", &v[i]);
	} 
	//paso 1-> la norma, suma de valores cuadrados
	for(j=0;j<n;j++){
	sum+=pow(v[j],2);
	
	}
 	
 	printf("la suma de los cuadrados es %f\n", sum);
 	//paso2->la raiz de los valores al cuadrado
 	root=sqrt(sum);
 	printf("la raiz de la suma de los cuadrados es %f\n", root);
 	
 	printf("El vector normalizado es:");
		//paso 3-> Normalizando el vectos y determinando que no sea nulo
 	if(root!=0){
	 
	 for(k=0;k<n;k++){
 		v2[k]=v[k]/root;
 		
 		
 		printf("%f,", v2[k]);
	 }
	}else{
		printf("error, todos los elementos del vector son cero");
	}
	

 	
}

