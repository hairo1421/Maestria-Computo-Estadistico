/*TAREA 6
EJERCICIO 1
VECTOR NORMALIZADO matriz dinamica y lectrura y escritura de archivos
HAIRO ULISES MIRANDA BLEMONTE
*/


# include<stdio.h>
# include<stdlib.h>
# include<math.h>
int main () {
// Declaraciones
int elementos, i,n,j,k;

FILE  * vec,*vecnorm;
// Verificaciones de archivos
if (( vec = fopen ("vector.txt","r")) == NULL ){
printf (" Error al abrir el archivo de entrada .");
return 1;
}

// Verificaciones de archivos
if (( vecnorm = fopen ("vectornorm.txt","w")) == NULL ){
printf (" Error al abrir el archivo de entrada .");
return 1;//leyendo el archivo
}

// Lectura de el vector
fscanf ( vec ,"%d" ,&elementos );//le el primer elemento que es el tamaño del vector
printf("%d\n", elementos);


//Declaracion y asignacion de memoria
double * v=( double *) calloc ( elementos , sizeof ( double *) );
double * v2=( double *) calloc ( elementos , sizeof ( double *) );
//tamaño de bites en un puntero que inicializo con ese tamaño
//tamaño de elementos que fue el primer elemento que leyó
// Lectura e inicializacion de la matriz

// Lectura e inicializacion de la matriz

for (j=0;j< elementos ;j ++)
fscanf ( vec ,"%lf" ,& v [j]) ;

for(i=0;i<elementos;i++){
	printf("%f ", v[i]);
}
	 //	Instrucciones .../////////////////////////////////

    float sum,root;
	//bubble method; almacenamiento de valores
	sum=0;
	root=0; 
	//declarar arrays putero que apunta a memoria de otro
	

	//paso 1-> la norma, suma de valores cuadrados
	for(i=0;i<elementos;i++){
	sum+=pow(v[i],2);
	}
	
 	
 	fprintf(vecnorm,"la suma de los cuadrados es %f\n", sum);
 	//paso2->la raiz de los valores al cuadrado
 	root=sqrt(sum);
 	fprintf(vecnorm,"la raiz de la suma de los cuadrados es %f\n", root);
 	
 	
		//paso 3-> Normalizando el vectos y determinando que no sea nulo
 	if(root!=0){
	 
	 for(k=0;k<elementos;k++){
 		v2[k]=v[k]/root;
 		
 		
 		fprintf(vecnorm,"%f,", v2[k]);
	 }
	}else{
		fprintf(vecnorm,"ERROR", v2[k]);
	}
	

	//cerrando files
	fclose(vec);
	fclose(vecnorm);

	// L i b e r a c i n de memoria asignada
	for ( i=0;i< elementos ;i++)
	free (v) ;
	free (v2) ;	
return 0;
}


