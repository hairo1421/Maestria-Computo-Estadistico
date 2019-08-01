/*TAREA 6
EJERCICIO 4
INVERSA DE UNA MATRIZ con memoria dinamica
HAIRO ULISES MIRANDA BLEMONTE
*/
# include<stdio.h>
# include<stdlib.h>

 int main(){
int renglones , columnas ,i,j;
FILE * matriz , * inversa ;
// Verificaciones de archivos
if (( matriz = fopen ("matriz.txt","r")) == NULL ){
printf (" Error al abrir el archivo de entrada .");
return 1;
}
if (( inversa = fopen ("inversa.txt","w") ) == NULL ){
printf (" Error al abrir el archivo de salida .");
return 1;
}
// Lectura de dimensiones de la matriz
fscanf ( matriz ,"%d %d" ,& renglones , & columnas );

// Declaracion y asignacion de memoria
double ** m =( double **) malloc ( renglones * sizeof ( double *) );
for (i=0;i< renglones ;i++)
m [i ]=( double *) malloc ( columnas * sizeof ( double ));
// Lectura e inicializacion de la matriz
for (i=0;i< renglones ;i++)
for (j=0;j< columnas ;j ++)
fscanf ( matriz ,"%lf" ,& m [i][j]) ;
for (i=0;i< renglones ;i++){
printf("\n");
for (j=0;j< columnas ;j ++){

printf("%lf " ,m [i][j]) ;
}
}

//determinante
	int a,b,c,aux1,aux2,aux3,det;
	
		//fila uno de l matriz
		a=(m[0][0]);
		b=(m[0][1]);
		b=((-1)*b);
		c=(m[0][2]);
	
		//matriz de cofactores por primer elemento de la fila
		aux1=(m[1][1]*m[2][2])-(m[1][2]*m[2][1]);
		aux1=a*aux1;
		//matriz de cofactores por segun elemento de la fila
		aux2=(m[1][0]*m[2][2])-(m[1][2]*m[2][0]);
		aux2=b*aux2;
		//matriz de cofactores por tercer elemento de la fila
		aux3=(m[1][0]*m[2][1])-(m[1][1]*m[2][0]);
		aux3=c*aux3;
		//valor del determinante
		det=aux1+aux2+aux3;
	
		printf("\nEl determinante de la matriz es:%i\n", det);
		
		
		if(det!=0){
	//clcular cofactores si el determinante es cero
			double cf[3][3];
			double inv[3][3];
			cf[0][0]=(m[1][1]*m[2][2]-m[1][2]*m[2][1]);
            cf[0][1]=(-1)*(m[1][0]*m[2][2]-m[1][2]*m[2][0]);
            cf[0][2]=(m[1][0]*m[2][1]-m[1][1]*m[2][0]);
            cf[1][0]=(-1)*(m[0][1]*m[2][2]-m[0][2]*m[2][1]);
            cf[1][1]=(m[0][0]*m[2][2]-m[0][2]*m[2][0]);
            cf[1][2]=(-1)*(m[0][0]*m[2][1]-m[0][1]*m[2][0]);
            cf[2][0]=(m[0][1]*m[1][2]-m[0][2]*m[1][1]);
            cf[2][1]=(-1)*(m[0][0]*m[1][2]-m[0][2]*m[1][0]);
            cf[2][2]=(m[0][0]*m[1][1]-m[0][1]*m[1][0]);
            	//transpuesta della matriz de cofactores
         
           		for(i=0;i<3;i++){
	   	 			for(j=0;j<3;j++){
				printf("%f  ",cf[j][i]);
        		}
        		printf("\n");
				}
				printf("\n");
				for(i=0;i<3;i++){
	   	 			for(j=0;j<3;j++){
				//escribir en archivo txt la inversa
				fprintf(inversa,"%f  ",cf[j][i]/det);
        		}
        		fprintf(inversa,"\n");
				}
		
			}else{
				printf("no tiene inversa");
			}

// L i b e r a c i n de memoria asignada
for ( i=0;i< renglones ;i++)
free ( m [i]) ;
free ( m );
 
	 
	 
 }


 
 
 

