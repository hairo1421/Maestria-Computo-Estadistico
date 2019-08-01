/*EJERCICIO 4
Abre cualquier txt e indica numero de caracteres sin guardarlos en array
HAIRO ULISES MIRANDA BLEMONTE
*/

#include <stdio.h>
#include <string.h>

int main(){;
    FILE *texto;
    texto= fopen("Documento.txt","r"); // se abre como solo lectura
    if ( texto == NULL ){
	printf (" Error al abrir el archivo .");
	}
    
    int cont=0;
	
	while((fgetc(texto))!=EOF)  { //recorre todo el txt y el contador me dirá cuantos caracteres
    cont++;
}	printf("\n\n  El total de caracteres es: %i",cont);

	fclose(texto);
        return 0;
    }
