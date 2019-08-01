/*Programación y Análisis de Algoritmos
**TAREA 4: CALCULAR DIAS ENTRE DOS FECHAS
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**05 DE SEPTIEMBRE DEL 2018*/

/* Limitante de código
/* Este código sólo permite calcular fechas cuyo inicio es menor a la segunda fecha*/
/* Se utiliza "do while" en repetidas ocasiones, por lo tanto, sensible a un infinit loop*/

#include<stdio.h>


typedef struct { /*Estructura*/
	
	int dia;
	int mes;
	int year;
	
}calendario;

/*Prototipos de las funciones*/

void pedirFecha(calendario *fecha1, calendario *fecha2); /* ingresa dos fechas para contar los días entre ellas*/
void caso1(calendario *fecha1, calendario *fecha2); /*CASO 1: mes y año distinto entre fechas*/
void caso2(calendario *fecha1, calendario *fecha2); /*CASO 2: mismo año entre las dos fechas*/
void caso3(calendario *fecha1, calendario *fecha2); /*CASO 3: dia, mes y años distinto entre las dos fechas*/

int main()
{
	
	calendario fecha1, fecha2; /*Declarando estructura*/
	
	pedirFecha(&fecha1, &fecha2); /* ingresa dos fechas para contar los días entre ellas*/
	
	if(fecha1.mes==fecha2.mes && fecha1.year==fecha2.year ){ /*CASO 1: mes y año distinto entre fechas*/
		caso1(&fecha1, &fecha2);
	} else if (fecha1.year==fecha2.year){ /*CASO 2: mismo año entre las dos fechas*/
		caso2(&fecha1, &fecha2);
	} else { /*CASO 3: dia, mes y años distinto entre las dos fechas*/
		caso3(&fecha1, &fecha2);
	} //end if
		
	return 0;
	
} // end main

/*
 * Función: caso1
 * ----------------------------
 *   Paso por referencia estructura. cuenta dias entre fechas cuando el  mes y año
 *	 son iguales.
 
 *   calendario *fecha1: puede ser día, mes y año 
 *   calendario *fecha2: puede ser día, mes y año
 *
 *   Función tipo void. Imprime los días que pasaron entre dos fechas
 */

void caso1(calendario *fecha1, calendario *fecha2 )
{ /*CASO 1: mes y año iguales entre fechas*/
	
	printf("Dias que trascurrieron: %d", fecha2->dia - fecha1->dia); /* resta dias entre dos fechas con el mismo mes y año*/
	
} // end caso1

/*
 * Función: caso2
 * ----------------------------
 *   Paso por referencia estructura. cuenta dias entre fechas cuando el año entre fechas son las mismas
 *	 
 
 *   calendario *fecha1: puede ser día, mes y año 
 *   calendario *fecha2: puede ser día, mes y año
 *
 *   Función tipo void. Imprime los días que pasaron entre dos fechas
 */

void caso2(calendario *fecha1, calendario *fecha2)
{ /*CASO 2: mismo año entre las dos fechas*/

	/* variables auxiliares*/
	int i = 0;int j = 0;int aux = 0;
	/* contador de dias entre fechas*/
	int count = 0;
	/* array con fechas de finales de mes*/
	int finMes[4] = {29, 30 ,31, 28};
	
	/* Resta  días. dia fecha uno menos el último día del mes de fecha 1 */
	/* Esto hace que pasemos al proximo mes de ese mismo año (para fecha1)*/
	if(fecha1->mes==2){ 
		if(fecha1->mes == 2 && (fecha1->year % 4 == 0 && fecha1->year % 100 != 0 || fecha1->year % 400 == 0)){
			count = finMes[3] - fecha1->dia;	
		} else if (fecha1->mes == 2 && (fecha1->year % 4 == 0 && fecha1->year % 100 != 0 || fecha1->year % 400 == 0)){
			count = finMes[0] - fecha1->dia;	
		}
	} else if (fecha1->mes == 4  || fecha1->mes == 6 || fecha1->mes == 9 || fecha1->mes == 11){
		count = finMes[1] - fecha1->dia;
	} else {
		count = (finMes[2]-fecha1->dia);	
	} // end else
			printf("\n%d\n",count);
			
	/*Recorre meses y días, de fecha 1 más lo que ya se contó, hasta un mes antes, último día, de fecha dos*/
	 
	for(j = fecha1->mes+1; j <= fecha2->mes-1; j++){ 
	
		if(fecha1->mes == 2 && (fecha1->year % 4 == 0 && fecha1->year % 100 != 0 || fecha1->year % 400 == 0)){
			for(i = 1; i<= 28; i++){
				count++;
			} // end for
		} else if (fecha1->mes == 2 && (fecha1->year % 4 != 0 && fecha1->year % 100 == 0 || fecha1->year % 400 != 0)){
			for(i = 1; i<= 29; i++){
				count++;
			}
		}else if (fecha1->mes == 4 || fecha1->mes == 6 || fecha1->mes == 9 || fecha1->mes == 11){
			for(i = 1; i <= 30; i++){ 
				count++;
			} // end for
		} else {
			for(i = 1; i <= 31; i++){
		    	count++;
			} // end for	
		} // end else	
	} // end for	
		 	
	
	/*suma del dia 1 del mes de fecha 2, al dia que quiero de la segunda fecha*/
	aux = fecha2->dia-1; 
	count += aux;
	
	printf("Dias que trascurrieron: %d\n",count); /*días que pasaron entre fechas*/
	
} // end caso2

/*
 * Función: caso3
 * ----------------------------
 *   Paso por referencia estructura. cuenta dias entre fechas cuando el dia, mes y año son distintos
 *	 
 
 *   calendario *fecha1: puede ser día, mes y año 
 *   calendario *fecha2: puede ser día, mes y año
 *
 *   Función tipo void. Imprime los días que pasaron entre dos fechas
 */

void caso3(calendario *fecha1, calendario *fecha2){ /*CASO 3: dia, mes y años distinto entre las dos fechas*/
	
	/* variables auxiliares*/
	int i = 0;int j = 0;int w = 0;int x = 0;
	int y = 0;int aux = 0;
	/* contador de dias entre fechas*/
	int count = 0;
	/* array con fechas de finales de mes*/
	int finMes[4] = {29, 30 ,31, 28};

	
	/* Resta  días. dia fecha uno menos el último día del mes de fecha 1 */
	/* Esto hace que pasemos al proximo mes de ese mismo año (para fecha1)*/	
	if(fecha1->mes==2){ 
		if(fecha1->mes == 2 && (fecha1->year % 4 == 0 && fecha1->year % 100 != 0 || fecha1->year % 400 == 0)){
			count = finMes[3] - fecha1->dia;	
		} else if (fecha1->mes == 2 && (fecha1->year % 4 == 0 && fecha1->year % 100 != 0 || fecha1->year % 400 == 0)){
			count = finMes[0] - fecha1->dia;	
		}
	} else if (fecha1->mes == 4  || fecha1->mes == 6 || fecha1->mes == 9 || fecha1->mes == 11){
		count = finMes[1] - fecha1->dia;
	} else {
		count = (finMes[2]-fecha1->dia);	
	} // end else
	
	/*Recorre meses y días, de fecha 1 más lo que ya se contó, hasta ultimo mes de un año, último días de ese mes*/
	/*Esto con el fin de iniciar en un nuevo año 1/1/año*/
	for(j = fecha1->mes+1; j <= 12; j++){ 
	
		if(fecha1->mes == 2 && (fecha1->year % 4 == 0 && fecha1->year % 100 != 0 || fecha1->year % 400 == 0)){
			for(i = 1; i<= 28; i++){
				count++;
			} // end for
		} else if (fecha1->mes == 2 && (fecha1->year % 4 != 0 && fecha1->year % 100 == 0 || fecha1->year % 400 != 0)){
			for(i = 1; i <= 29; i++){
				count++;
			}
		}else if (fecha1->mes == 4 || fecha1->mes == 6 || fecha1->mes == 9 || fecha1->mes == 11){
			for(i = 1; i <= 30; i++){ 
				count++;
			} // end for
		} else {
			for(i = 1; i <= 31; i++){
		    	count++;
			} // end for	
		} // end else	
	} // end for	
	
		 	
	/*comienza con dia 1, mes 1, del año siguiente al que se declara la primera fecha,*/
	/* hasta un años antes de la fecha 2 último mes, último días*/		 
	for(j = fecha1->year+1; j <= fecha2->year-1; j++){ 
		for(i = 1; i <= 12; i++){
			if(i == 2 && (fecha1->year % 4 == 0 && fecha1->year % 100 != 0 || fecha1->year % 400 == 0)){ //ahora i es mi fecha mes
	 			for(w = 1; w <= 28; w++){
					count++;
				} // end for		
		 	} else if (i == 2 && (fecha1->year % 4 != 0 && fecha1->year % 100 == 0 || fecha1->year % 400 != 0)){ //ahora i es mi fecha mes
	 			for(w = 1; w <= 29; w++){
					count++;
				} // end for
			} else if (i == 4 || i == 6|| i == 9 || i == 11){
		 		for(x = 1; x <= 30; x++){
					count++;
				} // end for
			} else {
				for(y = 1; y <= 31; y++){
					count++;
				} // end for	
			} // end else	
		} // end else 	
	} // end for
		
	/*comienza en en el primer dia, primer mes de la segunda fecha y */
	/*cuenta hasta el mes que quiera de ese año */		
	for(j = 1;j <= fecha2->mes-1; j++){ 
 		if(j == 2 && (fecha1->year % 4 == 0 && fecha1->year % 100 != 0 || fecha1->year % 400 == 0)){
 			for(i = 1; i <= 28; i++){
				count++;
			} // end for
		} else if (j == 2 && (fecha1->year % 4 != 0 && fecha1->year % 100 == 0 || fecha1->year % 400 != 0)){
 			for(i = 1; i <= 29; i++){
				count++;
			} // end for
		} else if (j == 4  || j == 6 || j == 9 || j == 11){ 
			for(i = 1; i <= 30; i++){
				count++;
			} // end count
		} else {
			for(i = 1; i <= 31; i++){
				count++;
			} // end for	
		} // end else	
	} // end for 	
			
		
	/*suma del dia 1 al dia que quiero de la segunda fecha. */
	aux = (fecha2->dia) - 1;
	count = count + aux;
			
	printf("Dias que trascurrieron: %d",count); /*días que pasaron entre fechas*/
			
} // end caso3

/*
 * Función: pedirFecha
 * ----------------------------
 *   Paso por referencia estructura. Pide números racionales
 *	
 *   calendario *fecha1: puede ser día, mes y año 
 *   calendario *fecha2: puede ser día, mes y año
 *
 *   Función tipo void. Imprime la fecha que ingresó el usuario en formato dd/mm/año
 */

void pedirFecha(calendario *fecha1, calendario *fecha2)
{  
	// restringir  año y if para febrero e if para los de 30 e if para los de 31
	do{
		do{
			/*Primera fecha (AÑO)*/
			printf("\nProporciona una fecha de inicio (Anio a cuatro digitos):\n");
			scanf("%d", &fecha1->year);
		}while(fecha1->year <= 0 ); // end do while
		
		do{
			/*Segunda fecha(AÑO)*/
			printf("Proporciona una fecha final (Anio a cuatro digitos):\n");
			scanf("%d", &fecha2->year);
		}while(fecha2->year <= 0); // end do while
		
		if(fecha1->year > fecha2->year){
			printf("La fecha de inicio debe ser mayo a la final\n");
		} // end if
	}while ( fecha1->year > fecha2->year); // end do while
	
	do{
		do{
			/*Primera fecha (MES)*/
			printf("Proporciona una fecha de inicio (Mes a un digito si es de enero a septiembre):\n");
			scanf("%d", &fecha1->mes);
		}while(fecha1->mes > 12 || fecha1->mes <= 0);
		do{
			/*Segunda fecha(MES)*/
			printf("Proporciona una fecha final (Mes a un digito si es de enero a septiembre):\n");
			scanf("%d", &fecha2->mes);
		}while(fecha2->mes > 12 || fecha2->mes <= 0);
		if(fecha1->year == fecha2->year && fecha1->mes > fecha2->mes){
			printf("La fecha de inicio debe ser mayo a la final\n");
		}
	}while(fecha1->year == fecha2->year && fecha1->mes > fecha2->mes);
	
	/*Primera fecha (DIA)*/
	if(fecha1->mes == 2 && (fecha1->year % 4 == 0 && fecha1->year % 100 != 0 || fecha1->year % 400 == 0)){
		do{
			printf("Proporciona una fecha de inicio (Dia):\n");
			scanf("%d", &fecha1->dia);
		}while(fecha1->dia > 28 || fecha1->dia <= 0); // end do while
	} else if(fecha1->mes == 2 && (fecha1->year % 4 != 0 && fecha1->year % 100 == 0 || fecha1->year % 400 != 0)){
		do{
			printf("Proporciona una fecha de inicio (Dia):\n");
			scanf("%d", &fecha1->dia);
		}while(fecha1->dia > 29 || fecha1->dia <= 0); // end do while		
	} else if (fecha1->mes == 4 || fecha1->mes == 6 || fecha1->mes == 9 || fecha1->mes== 11){
		do{
			printf("Proporciona una fecha de inicio (Dia):\n");
			scanf("%d", &fecha1->dia);
		}while(fecha1->dia > 30 || fecha1->dia <= 0); // end do while
	} else {
			do{
			printf("Proporciona una fecha de inicio (Dia):\n");
			scanf("%d", &fecha1->dia);
		}while(fecha1->dia > 31 || fecha1->dia <= 0); // end do while
	} // end else
	
	/*Segunda fecha(DIA)*/
	if(fecha2->mes == 2){
		do{
			printf("Proporciona  fecha final (Dia):\n");
			scanf("%d", &fecha2->dia);
		}while(fecha2->dia > 29 || fecha2->dia <= 0); // end do while
	} else if (fecha2->mes == 4 || fecha2->mes == 6|| fecha2->mes == 9 || fecha2->mes== 11){
		do{
			printf("Proporciona  fecha final (Dia):\n");
			scanf("%d", &fecha2->dia);
		}while(fecha2->dia > 30 || fecha2->dia <= 0); // end do while
	} else {
			do{
			printf("Proporciona  fecha final (Dia):\n");
			scanf("%d", &fecha2->dia);
		}while(fecha2->dia > 31 || fecha2->dia <= 0); // end do while
	} // end else
		
	/*Imprimiendo fechas*/
	printf("Fecha de inicio es:%d/%d/%d\n", fecha1->dia, fecha1->mes, fecha1->year); 
	printf("Fecha final es:%d/%d/%d\n", fecha2->dia, fecha2->mes, fecha2->year);
	

		
}// end pedirFecha




