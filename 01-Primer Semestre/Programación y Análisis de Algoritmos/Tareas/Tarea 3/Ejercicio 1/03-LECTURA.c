/*Programación y Análisis de Algoritmos
**TAREA 3: INVENTARIO DE HERRAMIENTAS (parte 1. Leer el archivo).
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**01 DE SEPTIEMBRE DEL 2018*/

#include <stdio.h>

//LEER EL ARCHIVO



struct strucHerramienta {
   int id;
   char tool[30];
   int cantidad;
   double costo;
    
}; // end estructura

int main( ){
 	
 	FILE *hardWar; // apuntador del archivo
   int result; // para verificar si fread introdujó de manera correcta los bytes

   struct strucHerramienta herramienta = { 0, "", 0, 0.0 };
 
   if ( ( hardWar = fopen( "hardware.dat", "rb" ) ) == NULL ) { //verifica si abrió bien el archivo
      printf( "No se puede abrir el archivo." );
   } // end if
   else {
      printf( "%-10s%-30s%-12s%7s\n", "Registro", "Herramienta",
         "Cantidad", "Costo" ); //imprimiendo nombres de colunas

      // leer todos los registro incluyendo eof
      while ( !feof( hardWar ) ) {
         result = fread( &herramienta, sizeof( struct strucHerramienta ), 1, hardWar );

         // Mostrando la lista
         if ( result != 0 && herramienta.id != 0 ) {
            printf( "%-10d%-31s%-6d%10.2f\n\n",herramienta.id,herramienta.tool,herramienta.cantidad,herramienta.costo );
         } // end if
      } // end while

      fclose( hardWar ); // cerrar archivo
   } // end else
}// end main


