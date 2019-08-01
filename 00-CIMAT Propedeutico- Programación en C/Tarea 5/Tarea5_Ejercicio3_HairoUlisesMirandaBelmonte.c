/*TAREA 5
EJERCICIO 3
ESCRIBIR UN ARCHIVO Y CREAR SU TXT. ESCRIBIR AL FINAL ADEL ARCHIVO UN CARACTER
HAIRO ULISES MIRANDA BLEMONTE
*/
#include <stdio.h>
#include <string.h>
 int main(){
 	
 	char arch[50];
 	char texto[50];
 	printf("introduce nombre del archivo:");//introduzco nombre el txt
 	scanf( "%[^\n]%*c", &arch); 
 	printf("introduce lo que piensas:");//introdusco lo que irá al ultimo
 	scanf( "%[^\n]%*c", &texto); 
 	FILE * openarch;//abre el FILE
 	strcat(arch,".txt");//concateno nombre del archivo con .txt para generar el archivo
 	
 	openarch = fopen (arch,"a");//escribe al final
 	if ( openarch == NULL ){
		printf (" Error al abrir el archivo .");
			// El programa termina si el apuntador es nulo .
return 1;
}
fprintf (openarch ,"%s",arch );//me escribe al archivo
fprintf(openarch ,"\n");
fprintf (openarch ,"%s",texto );
 	
 	fclose(openarch);//ciero FILE

	return 0;
 }
 
 


