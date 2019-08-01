/*Programación y Análisis de Algoritmos
**TAREA 3: INVENTARIO DE HERRAMIENTAS (parte 1. Iniciar el archivo).
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**01 DE SEPTIEMBRE DEL 2018*/

#include <stdio.h>

//INICIALIZAR EL ARCHIVO

struct strucHerramienta {
   unsigned int id;
   char tool[30]; //herramientas
   int cantidad;
   double costo;
    

 
}; // end estructura

int main( )
{
	int i; // contador 
	
    // creando datos con información inicial según tipo de variable
   struct strucHerramienta  herramienta1= { 0, "", 0, 0.0 };

   FILE *hardWar; //hardware.dat apuntador al archivo

   // Inicializar el archivo. Se le anexa un ciclo que informa si abrió correctamente.
   if ( ( hardWar = fopen( "hardware.dat", "wb" ) ) == NULL ) {
      printf( "El archivo no se pudo abrir." );
   } // end if
   
   else {
      // pone los registros que se solicitan
      for ( i = 1; i <= 100; ++i ) {
         fwrite( &herramienta1, sizeof( struct strucHerramienta ), 1, hardWar );
      } // end for
}
  fclose( hardWar ); // cerrar el archivo
}
