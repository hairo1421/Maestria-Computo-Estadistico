/*Programación y Análisis de Algoritmos
**TAREA 3: INVENTARIO DE HERRAMIENTAS (parte 1. Ejercicio).
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**01 DE SEPTIEMBRE DEL 2018*/

#include <stdio.h>

//LISTA DE HERRAMIENTAS


struct strucHerramienta {
   int id;
   char tool[30];
   int cantidad;
   double costo;
}; // end estructura

//Prototipos de las fucniones
void iniciar();
void escribir();
void leer();
	
int opcion(  );
void texto( FILE *leer );
void actualiza( FILE *act );//modificar nombre, costo, cantidad
void nuevo( FILE *nue );
void borrar( FILE *borra );//eliminar herramienta

int main( void )
{
	
	iniciar();// inicia archivo aleatorio
	escribir();// redacta la lista de herramientas
	printf("\n");
	leer(); // lee para su modificación
	
   FILE *hardWar; // apuntador
	int choice; // eleccion del usuario

   if ( ( hardWar = fopen( "hardware.dat", "rb+" ) ) == NULL ) { //verificando apertura de archivo
      printf( "El archivo no se pudo abrir." );
   } // end if
  	else {
    	while ( ( choice = opcion() ) != 5 ) {//opciones
         switch ( choice ) {
            case 1:
               texto( hardWar );//generar archivo 
               break;
            case 2:
               actualiza( hardWar );//actualizar inventario 
               break;
            case 3:
               nuevo( hardWar );//agregar inventario
               break;
            case 4:
               borrar( hardWar );//eliminar inventario
               break;
            default:
               printf( "Error" );//te equivocaste
               break;
         } // end switch
      } // end while

      fclose( hardWar ); // cerrar archivo
   } // end else
} // end main

//formato txt
void texto( FILE *leer ){
   FILE *lista; // puntero
   int resultado; // usado para verificar si se leyeron bytes

   // estructura con información
   struct strucHerramienta  herramienta = { 0, "", 0, 0.0 };

   if ( ( lista = fopen( "lista.txt", "w" ) ) == NULL ) { //verifica si abre archivo
      printf( "El archivo no se puede abrir." );
   } // end if
   else {
      rewind( leer ); // pone el pointer al principio del archivo
      fprintf( lista,"%-10s%-30s%-12s%7s\n", "Registro", "Herramienta",
         "Cantidad", "Costo" );

      // Copia todos los registros del archivo de acceso aleatorio al archivo de texto
      while ( !feof( leer ) ) {
         resultado = fread(&herramienta, sizeof( struct strucHerramienta ), 1, leer);

         // Escribe un registro en texto
         if ( resultado != 0 && herramienta.id != 0 ) {
            fprintf( lista,"%-10d%-31s%-6d%10.2f\n\n",herramienta.id,herramienta.tool,herramienta.cantidad,herramienta.costo );
         } // end if
      } // end while

      fclose( lista ); //cierra el archivo
   } // end else
} // end texto

// actualiza la lista de herramientas
void actualiza( FILE *act ){
   int reg; //registro de la herramienta
   double precio; // costo de la herramienta
   int cantidad; // cantidad de la herramienta

   struct strucHerramienta herramienta = { 0, "", 0, 0.0 };

   // Actualiza registro existente
   printf( "%s", "Numéro del registro a actualizar (del 1 al 100): " );
   scanf( "%d", &reg );

   // Mueve el puntero al registro correo 
   fseek( act, ( reg - 1 ) * sizeof( struct strucHerramienta ),
      SEEK_SET );
   // lee registro 
   fread( &herramienta, sizeof( struct strucHerramienta ), 1, act );

   if ( herramienta.id == 0 ) {
      printf( "registo  #%d no contiene informacion.\n", reg ); // registro no existe
   } // end if
   else { 
      printf( "%-10d%-31s%-6d%10.2f\n\n",herramienta.id,herramienta.tool,herramienta.cantidad,herramienta.costo );

      printf( "%s", "Nuevo costo utilice + o - para cambiar costos: " );
      scanf( "%lf", &precio );
      herramienta.costo += precio; // nuevo costo
		
		printf( "%s", "Nueva cantidad (utilice + o - para cambiar cantidades): " );
      scanf( "%d", &cantidad );
      herramienta.cantidad += cantidad; // nuevo precio
      
	   printf( "%-10d%-31s%-6d%10.2f\n\n",herramienta.id,herramienta.tool,herramienta.cantidad,herramienta.costo );

      // mueva el puntero al lugar correcto
      fseek( act, ( reg - 1 ) * sizeof( struct strucHerramienta ),
         SEEK_SET );

      // escriba la actualizacion sobre el antiguo
      fwrite( &herramienta, sizeof( struct strucHerramienta ), 1, act );
   } // end else
} // end actualiza

// borra un registro
void borrar( FILE *borra ){
   struct strucHerramienta herramienta; // guarda el registro leido
   struct strucHerramienta herramienta1 = { 0, "", 0 , 0 }; // cliente blank

   int identificador; // numero cuenta

   // numero de cuenta a borrar
   printf( "%s", "Introduzca el numero de regitro de la herramienta ( del 1 al 100 ): " );
   scanf( "%d", &identificador );

   // mover el apuntador al registro adecuado
   fseek( borra, ( identificador - 1 ) * sizeof( struct strucHerramienta ),
      SEEK_SET );

   // lee registro del archivo
   fread( &herramienta, sizeof( struct strucHerramienta ), 1, borra );

   // muestra error si no existe
   if (herramienta.id == 0 ) {
      printf( "El registro %d no existe.\n", identificador );
   } // end if
   else { // borra el registro
      // mover el puntero al lugar correcto
      fseek( borra, ( identificador - 1 ) * sizeof( struct strucHerramienta ),
         SEEK_SET );

      // reemplaza el registro con registro en blanco
      fwrite( &herramienta1,
         sizeof( struct strucHerramienta ), 1, borra );
   } // end else
} // end function borraRegistro

// crear nuevo registro de herramienta
void nuevo( FILE *nue ){
 	//dando valores iniciales a la estructura según su tipo
   struct strucHerramienta herramienta = { 0, "", 0, 0.0 };
	
	int identificador; // número de registro

   // introducir nuevo registro de la herramienta
   printf( "%s", "Introduzca nuevo registro ( del1al100 ): " );
   scanf( "%d", &identificador );

   // mueva el puntero al lugar correcto del archivo
   fseek( nue, ( identificador - 1 ) * sizeof( struct strucHerramienta ),
      SEEK_SET );//busacr de arriba hacia abajo y repetir

   // leer informacion del archivo
   fread( &herramienta, sizeof( struct strucHerramienta ), 1, nue );

   // Decir que el registro ya existe
   if ( herramienta.id != 0 ) {
      printf( "registro #%d ya tiene informacion.\n",
         herramienta.id);
   } // end if
   else { 
      // crer registro de herramienta nueva
      printf( "%s", "Introduzca Nombre de la Herramienta, Cantidad, Costo\n? " );
       scanf( "%30s%4d%lf", herramienta.tool, &herramienta.cantidad, &herramienta.costo );
       
      herramienta.id = identificador; //valor que se le da a la estuctura  con elemento id

      // Mueve el puntero al registro correcto
      fseek( nue, ( herramienta.id - 1 ) *
         sizeof( struct strucHerramienta ), SEEK_SET );

      // inserta el registro
      fwrite( &herramienta,
         sizeof( struct strucHerramienta ), 1, nue );
   } // end else
} // end nuevo

// Menu
 int opcion(  ){
    int menu; // opciones
    
   // muestra opciones
   printf( "%s", "\nIntroduza opcion\n"
      "1 - guarde un archivo de lista llamado\n"
      "    \"lista.txt\" para imprimir\n"
      "2 - actualice herramienta\n"
      "3 - ingrese nueva herramienta\n"
      "4 - borre una herramienta\n"
      "5 - finalizar\n* " );

   scanf( "%u", &menu ); 
   return menu; // regresa la opción tecleada
} // end menu

void iniciar( ) {
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

void escribir( ){
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

void leer( ){
 	
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

