/*Programación y Análisis de Algoritmos
**TAREA 3: INVENTARIO DE HERRAMIENTAS (parte 1. Escribir en el archivo).
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**01 DE SEPTIEMBRE DEL 2018*/

#include <stdio.h>

//ESCRIBIR EN EL ARCHIVO



struct strucHerramienta {
   int id;
   char tool[30];
   int cantidad;
   double costo;
    

 
}; // end estructura

int main( )
{
  FILE *hardWar; // apuntador del archivo

   // creando datos con información inicial según tipo de variable
   struct strucHerramienta herramienta = { 0, "", 0, 0.0 };;
  
    // Inicializar el archivo. Se le anexa un ciclo que informa si abrió correctamente.
   if ( ( hardWar = fopen( "hardware.dat", "rb+" ) ) == NULL ) {
      printf( "Archivo no se puede abrir." );
   } // end if
   else {
      // Pesir el registro de la herramienta
      printf( "%s", "Introduzca el numero de herramienta #Registro"
         " el registro es un número del 1 al 100. Ingrese cero (0) para salir)\n? " );
      scanf( "%d", &herramienta.id	 );
   
      

      //Introducir los datos
      while (herramienta.id != 0 ) { //mientras sea distinto a cero, haz:
         printf( "%s", "Introduzca Nombre de la Herramienta, Cantidad, Costo\n? " ); 
         fscanf( stdin, "%30s%4d%lf", herramienta.tool,
            &herramienta.cantidad, &herramienta.costo ); //ingresando a memoria la información

         // busca la posicion 
         fseek( hardWar, ( herramienta.id - 1 ) *
            sizeof( struct strucHerramienta ), SEEK_SET ); //buscar de arriba hacia abajo y repetir

         // asignando espacio que es 1 para escribir
         fwrite( &herramienta, sizeof( struct strucHerramienta ), 1, hardWar );

         // Opción se introducir el registro de otra herramienta
         printf( "%s", "Introduzca numero de registro\n? " );
         scanf( "%d", &herramienta.id );
      } // end while
       fclose( hardWar );// cerrar el archivo
  }

}
