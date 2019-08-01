/*Programación y Análisis de Algoritmos
**PROYECTO FINAL: Rcpp y C. APLICACIÓN
** A TÉCNICA DE REMUESTREO BOOTSTRAP
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**20 DE NOVIEMBRE DEL 2018*/


/* Headers necesarios para el funcionamiento*/
#include <Rcpp.h>
using namespace Rcpp;

/*
 * Función: quicksort
 * ----------------------------
 *   Algoritmo de ordenamiento Quicksort
 *	 Ordenamiento de menos a mayor.
 *
 *	 Input: xx1 -> Vector Númerico Double (Datos)
 * 			 l  -> Entero que recibe el 0
 *			n1  -> Tamaño de muestra menos uno.
 *  
 *   Return: - 
 *
 *   Función tipo Void.
 */
 
/* Header para la exportacion */
// [[Rcpp::export]]
void quicksort(NumericVector xx1, int l, int r)
{
	/*Variables*/
	double v,i,j,t;
	
	/*Ordenamiento*/
	if (r > l){
    	v = xx1[r];
    	i= l-1;
    	j = r;
    	for(;;){
       		while(xx1[++i] < v);
        	while(xx1[--j] > v);

        	if(i >= j)
            break;
        	t=xx1[i];
        	xx1[i]=xx1[j];
        	xx1[j]=t;
    	}/*end for*/

    	t=xx1[i];
    	xx1[i]=xx1[r];
    	xx1[r] = t;

    	quicksort(xx1,l,i-1);
    	quicksort(xx1,i+1,r);
    	
	}/*end if*/
	
} // end quicksort

/*
 * Función: media
 * ----------------------------
 *   Estima media muestrañ
 *
 *	 Input: xx1 -> Vector Númerico Double (Datos)

 *   Return: variable double con media muestral 
 *
 *   Función tipo double.
 */

/* Header para la exportacion */
// [[Rcpp::export]]
double  media(NumericVector xx1)
{
	/**Variables*/
	int i = 0;
	double auxSuma = 0.0;
	double mediaBostrap = 0.0;
	
	/*Tamaño de muestra*/
	int n1 = xx1.size();
	
    /*Media*/                 
	for (i = 0; i < n1; i++) {
 		auxSuma = auxSuma + xx1[i];
	}/*end for*/
	
 	mediaBostrap = auxSuma / n1;
 	
	return mediaBostrap; /*Regresa Media*/
                     
} // end media

/*
 * Función: varianza
 * ----------------------------
 *   Estima varianza muestral.
 *
 *	 Input: xx1 -> Vector Númerico Double (Datos)
 *
 *   Return: variable double con varianza muestral 
 *
 *   Función tipo double.
 */

/* Header para la exportacion */
// [[Rcpp::export]]

double varianza(NumericVector xx1)
{	
	/*Variables*/	
 	double mediaBostrap = 0.0; int i = 0;
 	double auxVar = 0.0; double varBostrap = 0.0;
 	/*Tamaño de muestra*/
 	int n1 = xx1.size();
 	/*Media Datos*/
 	mediaBostrap = media(xx1);
 	
 	/*Suma diferencia de cuadrados*/
 	for (i = 0; i < n1; i++){
		auxVar = auxVar + pow((xx1[i] - mediaBostrap), 2);
  	}/*end for*/
  	
  	
 	varBostrap = auxVar / ((double)n1-1);
 	
  	return varBostrap; /*Regresa Varianza*/
  	
} // end varianza

/*
 * Función: sd
 * ----------------------------
 *   Estima desviación estándar muestral.
 *
 *	 Input: xx1 -> Vector Númerico Double (Datos)
 *
 *   Return: variable double con desviación estándar muestral 
 *
 *   Función tipo double.
 */

/* Header para la exportacion */
// [[Rcpp::export]]

double sd(NumericVector xx1)
{	
 	/*Utiliza función Varianza*/
  	return sqrt(varianza(xx1)); /*Regresa Desviación estándar*/
  	
} // end varianza

/*
 * Función: mediana
 * ----------------------------
 *   Estima mediana muestral.
 *
 *	 Input: xx1 -> Vector Númerico Double (Datos)
 *
 *   Return: variable double con mediana muestral 
 *
 *   Función tipo double.
 */
 
/* Header para la exportacion */
// [[Rcpp::export]]
double  mediana(NumericVector xx1)
{
	/*Tamaño de muestra*/
	int n1 = xx1.size();
	/*Algoritmo de ordenamiento Quicksort*/
	quicksort(xx1, 0, n1-1);
	/*Mediana. Tamaño de muestra par e impar*/
	if(n1%2==0) {
	return((xx1[n1/2] + xx1[n1/2 - 1]) / 2.0);
	} else {
	return xx1[n1/2];
	} /*end else*/    
	             
} // end mediana

/*
 * Función: bootstrapVarianza
 * ----------------------------
 *   Estima error estándar del estádistico de la varianza  muestral.
 *
 *	 Input: xx1 -> Vector Númerico Double (Datos)
 *			 B  -> Variable tipo entero con el número de simulación
 *
 *   Return: Lista con dos outputs.
 *			 1) Error estándar estimado del estadístico 
 *			 1) Estadísticos bootstrap generado por remuestreo bootstrap
 *
 *   Función tipo List.
 */

// Rcpp wrapper for C++ function
// [[Rcpp::export]]
                        
 List bootstrapVarianza(NumericVector x1, int B)
 { 
 	/*Variables*/
	int n1 = 0;  int simulacion = 0; int i = 0; int j = 0; 
	double auxSuma = 0.0; double mediaBostrap = 0.0;
	double auxVar = 0.0; double varBostrap = 0.0; 
	double sdBostrap = 0;
    /*Tamaño de Muestra*/                     
	n1 =  x1.size(); 
	/*Número de simulación*/  
	simulacion = B; 
	/*Vector guarda remuestreo*/
	NumericVector xx1(n1);
	/*vector guarda las B estimaciones del estadístico*/
	NumericVector estBoot(B);

	time_t t;
	srand((unsigned)time(&t));
	
	/*Re-muestreo boostrapt tamaño n1, B veces*/
	for (i = 0; i < simulacion; i++){
		for(j=0;j< n1;j++){
			xx1[j] = x1[rand()%n1];
 		} /*end for*/
 		estBoot[i] = varianza(xx1);
	}/*end for*/
	
	/*Media de los B estadísticos*/ 
 	for (i = 0; i < simulacion; i++){
		auxSuma = auxSuma + estBoot[i];
 	}
	
	mediaBostrap = auxSuma / simulacion;
 
	/*Suma de cuadrados de los B estadísticos*/ 
	for (i = 0; i < simulacion; i++){
		auxVar = auxVar + pow((estBoot[i] - mediaBostrap), 2);
 	}
 	
 	/*Varianza estimada por bootsrap*/ 
	varBostrap = auxVar / (simulacion);
 	/*error estándar estimado por bootsrap*/
	sdBostrap = sqrt(varBostrap);
 	/*Regresa lista con error estandar del estadístico y estadísticos del remuestreo*/
	 return Rcpp::List::create(Rcpp::Named("se") = sdBostrap,
                          Rcpp::Named("tboot") = estBoot); 

 }/*end bootstrapVarianza*/
 
 /*
 * Función: bootstrapSD
 * ----------------------------
 *   Estima error estándar del estádistico de la desviación estándar  muestral.
 *
 *	 Input: xx1 -> Vector Númerico Double (Datos)
 *			 B  -> Variable tipo entero con el número de simulación
 *
 *   Return: Lista con dos outputs.
 *			 1) Error estándar estimado del estadístico 
 *			 1) Estadísticos bootstrap generado por remuestreo bootstrap
 *
 *   Función tipo List.
 */

// Rcpp wrapper for C++ function
// [[Rcpp::export]]
                        
 List bootstrapSD(NumericVector x1,  int B)
 { 
 	/*Variables */
	int n1 = 0;  int simulacion = 0; int i = 0; int j = 0; 
	double auxSuma = 0.0; double mediaBostrap = 0.0;
	double auxVar = 0.0; double varBostrap = 0.0; 
	double sdBostrap = 0.0;
    /*Tamaño de la muestra*/                     
	n1 =  x1.size();   
	/*Tamaño de la simulació*/ 
	simulacion = B; 
	/*Vector remuestreo*/ 
	NumericVector xx1(n1);
	/*Vecttor estadísitcos bootstrap*/ 
	NumericVector estBoot(B);  
	
	time_t t;
	srand((unsigned)time(&t));
	/*Bootstrap*/ 
	for (i = 0; i < simulacion; i++){
		for(j=0;j< n1;j++){
			xx1[j] = x1[rand()%n1];
	} /*end for*/
 	
	estBoot[i] = sd(xx1);
	}/*end for*/
	
	/*Medias bootstrap*/ 
 	for (i = 0; i < simulacion; i++){
		auxSuma = auxSuma + estBoot[i];
	}/*end for*/
	
	mediaBostrap = auxSuma / simulacion;
	/*Suma de cuadrados bootstrap*/ 
	for (i = 0; i < simulacion; i++){
		auxVar = auxVar + pow((estBoot[i] - mediaBostrap), 2);
 	}/*end for*/
 	/*Varianza estimada por bootsrap*/
 	varBostrap = auxVar / (simulacion);
 	/*error estándar estimado por bootsrap*/
	sdBostrap = sqrt(varBostrap);
 	/*Regresa lista con error estandar del estadístico y estadísticos del remuestreo*/
	return Rcpp::List::create(Rcpp::Named("se") = sdBostrap,
                          Rcpp::Named("tboot") = estBoot);

 } /*end bootstrapSD*/
 
  /*
 * Función: bootstrapMediana
 * ----------------------------
 *   Estima error estándar del estádistico de la mediana  muestral.
 *
 *	 Input: xx1 -> Vector Númerico Double (Datos)
 *			 B  -> Variable tipo entero con el número de simulación
 *
 *   Return: Lista con dos outputs.
 *			 1) Error estándar estimado del estadístico 
 *			 1) Estadísticos bootstrap generado por remuestreo bootstrap
 *
 *   Función tipo List.
 */
 
 // Rcpp wrapper for C++ function
// [[Rcpp::export]]
                        
 List bootstrapMediana(NumericVector x1, int B)
 { 
	/*Variables*/
	int n1 = 0;  int simulacion = 0; int i = 0; int j = 0; 
	double auxSuma = 0.0; double mediaBostrap = 0.0;
	double auxVar = 0.0; double varBostrap = 0.0; 
	double sdBostrap = 0.0;
    /*Tamaño de Muestra*/                      
	n1 =  x1.size();
	   
	simulacion = B; 
	/*Número de simulación*/  
	NumericVector xx1(n1);
	/*Vector guarda remuestreo*/
	NumericVector estBoot(B); 
	
	time_t t;
	srand((unsigned)time(&t));
	/*Re-muestreo boostrapt tamaño n1, B veces*/
	for (i = 0; i < simulacion; i++){
		for(j=0;j< n1;j++){
		xx1[j] = x1[rand()%n1];
	} /*end for*/

 	estBoot[i] = mediana(xx1); /*ingresa una media por cada simulación*/
	} /*end for*/
	
 	/*Media de los B estadísticos*/
 	for (i = 0; i < simulacion; i++){	
		auxSuma = auxSuma + estBoot[i];
 	} /*end for*/
 	
	mediaBostrap = auxSuma / simulacion;
 	/*Suma de cuadrados de los B estadísticos*/
	for (i = 0; i < simulacion; i++){
		auxVar = auxVar + pow((estBoot[i] - mediaBostrap), 2);
 	}
 	/*Varianza estimada por bootsrap*/
 	varBostrap = auxVar / (simulacion);
 	/*error estándar estimado por bootsrap*/
 	sdBostrap = sqrt(varBostrap);
 	
	/*Regresa lista con error estandar del estadístico y estadísticos del remuestreo*/
 	return Rcpp::List::create(Rcpp::Named("se") = sdBostrap,
                          Rcpp::Named("tboot") = estBoot);
 } /*end bootstrapMediana*/
 
   /*
 * Función: bootstrapMedia
 * ----------------------------
 *   Estima error estándar del estádistico de la media  muestral.
 *
 *	 Input: xx1 -> Vector Númerico Double (Datos)
 *			 B  -> Variable tipo entero con el número de simulación
 *
 *   Return: Lista con dos outputs.
 *			 1) Error estándar estimado del estadístico 
 *			 1) Estadísticos bootstrap generado por remuestreo bootstrap
 *
 *   Función tipo List.
 */
 
 
 // Rcpp wrapper for C++ function
// [[Rcpp::export]]
                        
 List bootstrapMedia(NumericVector x1, int B)
 { 
	/*Variables*/
	int n1 = 0;  int simulacion = 0; int i = 0; int j = 0; /*Variables de apoyo*/
	double auxSuma = 0.0; double mediaBostrap = 0.0;
	double auxVar = 0.0; double varBostrap = 0.0; 
	double sdBostrap = 0.0;
    /*Tamaño de Muestra*/                 
	n1 =  x1.size();   
	/*Número de simulación*/
	simulacion = B; 
	/*Vector guarda remuestreo*/
	NumericVector xx1(n1);
	/*vector guarda las B estimaciones del estadístico*/
	NumericVector estBoot(B);
	
	time_t t;
	srand((unsigned)time(&t));
	/*Re-muestreo boostrapt tamaño n1, B veces*/
	for (i = 0; i < simulacion; i++){
		for(j=0;j< n1;j++){
			xx1[j] = x1[rand()%n1];
 	} /*end for*/

 	estBoot[i] = media(xx1);
	}/*end for*/
 	/*Media de los B estadísticos*/ 
 	for (i = 0; i < simulacion; i++){
	auxSuma = auxSuma + estBoot[i];
 	}/*end for*/

	mediaBostrap = auxSuma / simulacion;
 	/*Suma de cuadrados de los B estadísticos*/
	for (i = 0; i < simulacion; i++){
		auxVar = auxVar + pow((estBoot[i] - mediaBostrap), 2);
 	}/*end for*/
 	
 	/*Varianza estimada por bootsrap*/
	varBostrap = auxVar / (simulacion);
 	/*error estándar estimado por bootsrap*/
 	
	sdBostrap = sqrt(varBostrap);
 	/*Regresa lista con error estandar del estadístico y estadísticos del remuestreo*/
	return Rcpp::List::create(Rcpp::Named("se") = sdBostrap,
                          Rcpp::Named("tboot") = estBoot);

 }/*end bootstrapMedia*/
