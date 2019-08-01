/*Tarea 3
*Version 1
*Ejercicio 12
*Solicitar 3 números los cuales significan una fecha (día, mes, año). Comprobar que sea válida
la fecha, si no es válido que imprima un mensaje de error, y si es válida imprimir el mes con su
nombre.
*/
#include<stdio.h>

void date();
int main(){
	date();
	
}
void date(){
	int year,month, day,aux;
	
	
	do {
		printf("Introduzca el year:");
		scanf("%i", &year);
	}while(year<=0);
	
	do {
		printf("Introduzca el numero del mes:");
		scanf("%i", &month);
	}while(month<=0||month>12);
	
	do {
		printf("Introduzca el dia:");
		scanf("%i", &day);
	}while(day<=0||day>31);


switch (month ){
        case  1 :
        case  3 :
        case  5 :
        case  7 :
        case  8 :
        case 10 :
        case 12 : if ( day >= 1 && day <= 31 )
                    printf( "\n La fecha es correcta" );
                    else
                    printf( "\n La fecha no es correcta" );
                    break;

        case  4 :
        case  6 :
        case  9 :
        case 11 : if ( day >= 1 && day <= 30 )
                    printf( "\n  La fecha es correcta\n" );
                    else
                    printf( "\n  La fecha no es correcta\n" );
                    break;

        case  2 : if( year % 4 == 0 && year % 100 != 0 || year % 400 == 0 )
                        if ( day >= 1 && day <= 29 )
                              printf( "\n  La fecha es correcta\n" );
                          else
                              printf( "\n La fecha no es correcta\n" );
                      else
                          if ( day >= 1 && day <= 28 )
                              printf( "\n La fecha  es correcta\n" );
                          else
                              printf( "\n   La fecha  no es correcta\n" );
        }
    
	switch(month){
		case 1:printf("%i de Enero del %i",day, year);break;
		case 2:printf("%i de Febrero del %i",day, year);break;
		case 3:printf("%i de Marzo del %i",day, year);break;
		case 4:printf("%i de Abril del %i",day, year);break;
		case 5:printf("%i de Mayo del %i",day, year);break;
		case 6:printf("%i de Junio del %i",day, year);break;
		case 7:printf("%i de Julio del %i",day, year);break;
		case 8:printf("%i de Agosto del %i",day, year);break;
		case 9:printf("%i de Septiembre del %i",day, year);break;
		case 10:printf("%i de Octubre del %i",day, year);break;	
		case 11:printf("%i de Noviembre del %i",day, year);break;
		case 12:printf("%i de Diciembre del %i",day, year);break;
		}
	
}
