/*TAREA 6
EJERCICIO 5
TABLA DE CONTIGEN
HAIRO ULISES MIRANDA BLEMONTE
*/
#include<stdio.h>
#include<stdlib.h>
void main(){
    int i,j,hs=0,he=0,fs=0,fe=0;
    char renglon;
    int *v;
    FILE *n;
        n=fopen("tarea6_ej5.txt","r");
        if (n==NULL )
        {
        printf (" Error al abrir el archivo .");
        }
        else{
            
        char c[500];
        fscanf(n,"%s",c);
        fscanf(n,"%s",c);
        renglon=atoi(c);
        int **m=(int**)malloc(renglon*sizeof(int*));
          for (i=0;i< renglon ;i++)
            m[i]=( int *) malloc ( 2 * sizeof ( int ));
          for (i=0;i< renglon;i++)
          for (j=0;j< 2 ;j ++)
            fscanf (n,"%i",& m[i][j]);
       	    printf("el numerode muestra es: \n");
        	 for (i=0;i< renglon ;i++){
             for ( j=0;j< 2 ;j ++)
                    printf ( "%i ",m[i][j]);
                printf ("\n");
            }
            for (i=0;i< renglon;i++){
            	for (j=0;j< 2;j++)
             	if(j==0){
                
            if(m[i][j]==0){//caso de los hombres
                if(m[i][j+1]==0){//hombre sano
                    hs=hs+1;
                            }  else{
                          
            	if(m[i][j+1]==1){//hombre enfermo
                    he=he+1;
                     }
                    }
                  }
                    else{
       		            if(m[i][j]==1){//caso de los hombres
                                
                          if(m[i][j+1]==0){//mujer sana
                                    fs=fs+1;
                                }else{                                                  
                                if(m[i][j+1]==1){//mujer enferma
                            	fe=fe+1;
                            	}
                            }
                           }
                        }
                    }
            }
            printf("Tabla de contigencia:\n");

            printf("       Sano   Enfermo  Total\n");
            printf("Hombre  %i       %i        %i  \n",hs,he,hs+he);
            printf("Mujer   %i       %i        %i   \n",fs,fe,fs+fe);
            printf("Total   %i       %i        %i   \n",fs+hs,fe+he,fe+fe+fs+hs);

            for (i=0;i< renglon ;i++)
                free (m[i]);
            free (m);
        }
}
