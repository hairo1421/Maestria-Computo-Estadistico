##############################################################################################
##############################################################################################
##############################################################################################
########################################  TAREA 6 ############################################
##############################################################################################
##############################################################################################
##############################################################################################


##################
# Parámetros
##################

import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import integrate
from scipy.special import airy
from sympy import im
import os
sns.set_style("darkgrid")

path = 'C:\\Users\\h_air\\Documents\\Semestre 3\\Tópicos Selectos en Estadística\\Matrices aleatorias\\Tareas\\Tarea 6'
os.chdir(path)

#%%

##############################################################################################
##############################################################################################
##############################################################################################
########################  Ejercicio 1 . modelo general spiked ################################
##############################################################################################
##############################################################################################
##############################################################################################


##################
# Parámetros
##################
b = 1.0 
k = 4.0
sigma2_b = 0.01
sigma2_f = 0.000158
sigma2_e = 0.0045
n = 80
p = range(50,201)

#%%
##################
# Modelo teórico
##################

k = 4.
lambda_1 = [sigma2_f*i*((1.*b**2)*1.*k+sigma2_b)+sigma2_e for i in p]
lambda_2 = [sigma2_f*i*sigma2_b+sigma2_e for i in p]
lambda_3 = [sigma2_e for i in p]

#%%  
    
########################
# Plot teórico 
########################

plt.plot(list(p), lambda_1, label='lambda 1 teórica')
plt.plot(list(p), lambda_2, label='lambda 2 teórica')
plt.plot(list(p), lambda_3, label='lambda 3 teórica')
plt.title("Teórico")
plt.xlabel("# de valores en portafolio")
plt.ylabel("Eigenvalues")
plt.legend()
plt.savefig('Ej1_fig1.png')


#%%
####################
# Modelo Empírico 
####################
Matriz = np.zeros((151,5), dtype=float)
LOWER = np.zeros((151,5), dtype=float)
UPPER = np.zeros((151,5), dtype=float)

for i in p:      
    R_ = np.zeros((100, 5))
    for j in range(100):
        F = np.random.normal(0, np.sqrt(sigma2_f), (n, 4))
        L = np.random.normal(b,  np.sqrt(sigma2_b), (i, 4))
        epsilon =  np.random.normal(0,  np.sqrt(sigma2_e), (i, n))
        R = np.dot(L,F.T) + epsilon
        Sigma = np.dot(R,R.T) / (1.*n)
        v, w = np.linalg.eigh(Sigma)
        temp = np.sort(v)
        temp_2 = temp[::-1]
        R_[j,:] =  temp_2[0:5]
    Low =  np.quantile(R_[:,0], .25)
    Upp =  np.quantile(R_[:,0], .75)    
    vp = R_.mean(axis=0)
    Matriz[i-50,:] = vp
    LOWER[i-50,:] = Low
    UPPER[i-50,:] = Upp
  
#%%  
    
########################
# Plot Modelo Empírico 
########################
plt.plot(np.asarray(list(p)), Matriz[:,0], label='lambda 1 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,1], label='lambda 2 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,2], label='lambda 3 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,3], label='lambda 4 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,4], label='lambda 5 empirica')
plt.plot(np.asarray(list(p)), LOWER, c ="black")
plt.plot(np.asarray(list(p)), UPPER, c ="black")
plt.legend()
plt.title("The Journal of finance")
plt.xlabel("# de valores en portafolio")
plt.ylabel("Eigenvalues")
plt.savefig('Ej1_fig2.png')


#%%
############################################################
# Modelo Teórico Ajustado vs Teórico -> Lambda 1 calibrado 
############################################################

lambda_1modif = [(sigma2_f*i*((1.*b**2)*1.*k+sigma2_b)+sigma2_e)*(1+(1/n)*(sigma2_e/(sigma2_f*(sigma2_b+k*b**2)))) for i in p]
plt.plot(list(p), lambda_1modif, label='lambda 1 teórica calibrado')
plt.plot(list(p), lambda_1, label='lambda 1 teórica')
plt.title("Teórico vs Teórico calibrado")
plt.xlabel("# de valores en portafolio")
plt.ylabel("Eigenvalues")
plt.legend()

plt.savefig('Ej1_fig3.png')

#%%
##############################################################
# Modelo Teórico Ajustado -> Lambda 1 calibrado y Marcenko Pastur bound
##############################################################

lambda_k = [sigma2_f*i*((1.*b**2)*1.*k+sigma2_b)+sigma2_e  if i >= ((1/n)*(sigma2_e/(sigma2_f*sigma2_b))**2) else sigma2_e*(1+np.sqrt(i/n))   for i in p] 

plt.plot(list(p), lambda_k, "--", label='Marcenko Pastur bound',  color = "black")
plt.plot(list(p), lambda_1modif, label='lambda 1 teórico calibrado')
plt.title("Teórico calibrado")
plt.xlabel("# de valores en portafolio")
plt.ylabel("Eigenvalues")
plt.legend()


plt.savefig('Ej1_fig4.png')

#%%
#####################################
#  Teórico vs Teórico calibrado
#####################################

plt.plot(list(p), lambda_1modif,  label='lambda 1 teórica calibrado',  color = "black")
plt.plot(list(p), lambda_k, "--", label='Marcenko Pastur bound',  color = "black")
plt.plot(list(p), lambda_1, label='lambda 1 teórica')
plt.plot(list(p), lambda_2, label='lambda 2 teórica')
plt.plot(list(p), lambda_3, label='lambda 3 teórica')
plt.plot(np.asarray(list(p)), LOWER, c ="black")
plt.plot(np.asarray(list(p)), UPPER, c ="black")
plt.legend()
plt.title("The Journal of finance")
plt.xlabel("# de valores en portafolio")
plt.ylabel("Eigenvalues")

plt.savefig('Ej1_fig5.png')


#%%
############################################################
# Modelo Empírico vs Teórico calibrado
############################################################

plt.plot(np.asarray(list(p)), Matriz[:,0], label='lambda 1 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,1], label='lambda 2 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,2], label='lambda 3 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,3], label='lambda 4 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,4], label='lambda 5 empirica')
plt.plot(list(p), lambda_1modif,  label='lambda 1 teórica calibrado',  color = "black")
plt.plot(list(p), lambda_k, "--", label='Marcenko Pastur bound',  color = "black")
plt.plot(np.asarray(list(p)), LOWER, c ="black")
plt.plot(np.asarray(list(p)), UPPER, c ="black")
plt.legend()
plt.title("The Journal of finance")
plt.xlabel("# de valores en portafolio")
plt.ylabel("Eigenvalues")

plt.savefig('Ej1_fig6.png')

#%%
############################################################
# Modelo Empírico vs Teórico vs Teórico calibrado
############################################################

plt.plot(np.asarray(list(p)), Matriz[:,0], label='lambda 1 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,1], label='lambda 2 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,2], label='lambda 3 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,3], label='lambda 4 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,4], label='lambda 5 empirica')
plt.plot(list(p), lambda_1modif,  label='lambda 1 teórica calibrado',  color = "black")
plt.plot(list(p), lambda_k, "--", label='Marcenko Pastur bound',  color = "black")
plt.plot(list(p), lambda_1, label='lambda 1 teórica')
plt.plot(list(p), lambda_2, label='lambda 2 teórica')
plt.plot(list(p), lambda_3, label='lambda 3 teórica')
plt.plot(np.asarray(list(p)), LOWER, c ="black")
plt.plot(np.asarray(list(p)), UPPER, c ="black")
plt.legend()
plt.title("The Journal of finance")
plt.xlabel("# de valores en portafolio")
plt.ylabel("Eigenvalues")


plt.savefig('Ej1_fig7.png')

#%%
############################################################
# Modelo Empírico vs Teórico vs Teórico calibrado
############################################################

plt.plot(np.asarray(list(p)), Matriz[:,0], label='lambda 1 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,1], label='lambda 2 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,2], label='lambda 3 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,3], label='lambda 4 empirica')
plt.plot(np.asarray(list(p)), Matriz[:,4], label='lambda 5 empirica')
plt.plot(list(p), lambda_1modif,  label='lambda 1 teórica calibrado',  color = "black")
plt.plot(list(p), lambda_k, "--", label='Marcenko Pastur bound',  color = "black")
plt.plot(list(p), lambda_1, label='lambda 1 teórica')
plt.plot(list(p), lambda_2, label='lambda 2 teórica')
plt.plot(list(p), lambda_3, label='lambda 3 teórica')
plt.plot(np.asarray(list(p)), LOWER, c ="black")
plt.plot(np.asarray(list(p)), UPPER, c ="black")
plt.title("The Journal of finance")
plt.xlabel("# de valores en portafolio")
plt.ylabel("Eigenvalues")


plt.savefig('Ej1_fig8.png')

#%%
##############################################################################################
##############################################################################################
##############################################################################################
########################  Ejercicio 2 . MANOVA como Tracy-Widom ##############################
##############################################################################################
##############################################################################################
##############################################################################################

##############################################################################################
# a)
##############################################################################################

###############################################
#  Tracy-Widom Distribución beta = 2
###############################################


#########################
# Eq. en diff a resolver
#########################
def vdp1(t, y):
    return np.array([y[1], t*y[0] + 2*y[0]**3, y[3], y[0]**2 ])


#########################
# Parámetros
#########################
t0, t1 = 5, -8 # puntos intervalo
dx = .005 # discretización
A = airy(t0)[0]
B = airy(t0)[1]
y0 = [A, B, 0, A**2] # condiciones

#########################
# Resuelve
#########################
# Declara integral
r = integrate.ode(vdp1).set_integrator("dopri5")
# intervalo
t =np.arange(t0,t1,-dx)
# valores iniciales
r.set_initial_value(y0, t0)
# Resuelve
Temp = np.zeros((len(t), 4))
Temp[0,:] = y0
for i in range(1, len(t)):
    Temp[i,:] = r.integrate(t[i]) 

F2 = np.exp(-(np.array(Temp)[:,2])) # distribución
f2 = np.gradient(F2, t) # densidad

#%%

###############################################
#  Tracy-Widom Distribución beta = 1
###############################################

#########################
# Eq. en diff a resolver
#########################
def vdp2(t, y):
    return np.array([y[1], t*y[0] + 2*y[0]**3, -y[0], y[0]**2 ])

#########################
# Parámetros
#########################
t0, t1 = 5, -8  # puntos intervalo
dx = .005 # discretización
A1 = airy(t0)[0]
B1 = airy(t0)[1]
y0_1 = [A1, B1, 0, 0]  # condiciones

#########################
# Resuelve
#########################
# Declara integral
r1 = integrate.ode(vdp2).set_integrator("dopri5")
# intervalo
t_1 =np.arange(t0,t1,-dx)
# Resuelve
r1.set_initial_value(y0_1, t0)

Temp1 = np.zeros((len(t_1), 4))
Temp1[0,:] = y0_1
for i in range(1, len(t_1)):
    Temp1[i,:] = r1.integrate(t_1[i]) 
    

F1 = np.sqrt(F2)*np.exp(-.5*(np.array(Temp1)[:,2])) # distribución
f1 = np.gradient(F1, t_1) # densidad

#%%
#########################
# Distribución
#########################
plt.plot(t_1,F1, "g", label = "beta=1")
plt.plot(t_1,F2,"b", label = "beta=2")
plt.ylabel('f(s)')
plt.xlabel('s')
plt.title('Distribución Tracy-Widom')
plt.legend()

#plt.savefig('Ej2_fig1.png')






#%%
#########################
# Densidad
#########################

plt.plot(t_1,f1, "g", label = "beta=1")
plt.plot(t_1,f2,"b", label = "beta=2")
plt.ylabel('f(s)')
plt.xlabel('s')
plt.title('Densidad Tracy-Widom')
plt.legend()


#plt.savefig('Ej2_fig2.png')



#%%

##########################
# Distribuci´´on TW
##########################
f90 = t_1[F1>=.90].min()
f95 = t_1[F1>=.95].min()
f99 = t_1[F1>=.99].min()
f05 = t_1[F1>=(1-.05)].min()

##########################
# Parámetros
##########################
p =4
m=0
n=18.5
s= p
m2 =m
n2 = n
N = 2*(s+m2+n2)+1
gamma = 2*np.arcsin(np.sqrt((s-.5)/N))
phi = 2*np.arcsin(np.sqrt((s+ 2*m2+.5)/N))

##########################
# Formula
##########################
mu =2*np.log(np.tan((phi+gamma)/2))
sigma3 = (16/N**2)*(1/((np.sin(phi+gamma)**2)*np.sin(phi)*np.sin(gamma)))
sigma = sigma3**(1/3)
theta1 = np.exp(mu + f90*sigma)/(1+np.exp(mu + f90*sigma))
theta2 = np.exp(mu + f95*sigma)/(1+np.exp(mu + f95*sigma))
theta3 = np.exp(mu + f99*sigma)/(1+np.exp(mu + f99*sigma))
theta0 = np.exp(mu + f05*sigma)/(1+np.exp(mu + f05*sigma))

##########################
# Valor crítico TW
##########################
print(theta0)

#%%
##########################
# Visualización
##########################       
plt.plot(t_1, f1, color = "black")
plt.fill_between(t_1[t_1>0.652] ,0, f1[t_1> 0.652], color = "red")
plt.axvline(x=0.652, ymin = 0, ymax = .2, color='b', label = "theta")
plt.axvline(x=.377, ymin = 0, ymax = .2, color='g', label = "theta Obs")
plt.title('Prueba de hipótesis')
plt.ylabel('f(s)')
plt.xlabel('s')
plt.legend()


plt.savefig('Ej2_fig3.png')

#%%

##########################
# Visualización
##########################
plt.plot(t_1, f1, color = "black")
plt.fill_between(t_1[t_1>0.652] ,0, f1[t_1> 0.652], color = "red")
plt.axvline(x=0.652, ymin = 0, ymax = .2, color='b', label = "theta")
plt.axvline(x=theta0, ymin = 0, ymax = .2, color='black', ls = '--', label = "theta TW")
plt.title('Prueba de hipótesis')
plt.ylabel('f(s)')
plt.xlabel('s')
plt.legend()


plt.savefig('Ej2_fig4.png') 

#%%
##############################################################################################
# b)
##############################################################################################
# Descarga series


###############################################
# Series financieras Yahoo finances
###############################################

# Activos
Stock_Prices = ['AMD','AAPL','BAC','AMZN','T','GOOG','MO','DAL','AA','AXP','DD','BABA','ABT',
'UA','AMAT','AMGN','AAL','AIG','ALL','ADBE','GOOGL','ACN','ABBV','MT','LLY','AGN','APA','ADP',
'APC','AKAM','NLY','AEP','ATVI','ADSK','ADM','WBA','ARNA','LUV','ACAD','AN','AZN','AES','BUD','ARR','BDX','AKS',
'AB',
'ACOR',
'CS',
'AFL',
'ADI',
'ACIW',
'AMP',
'AVP',
'AMTD',
'AEO',
'AWK',
'NVO',
'ALK',
'PAA',
'ARCC',
'AAP',
'NAT',
'FNMA',
'AIV',
'AGIO',
'AEE',
'UBS',
'AVXL',
'ARLP',
'ANTM',
'AG',
'ABC',
'ATI',
'AVB',
'ALNY',
'LH',
'AVY',
'AUY',
'ASH',
'ANR',
'AINV',
'ACHN',
'ABMD',
'ABM']

#%%
##########################
# Donwload Series
##########################
import yfinance as yf
data = yf.download(Stock_Prices,"2018-04-20","2018-10-20") # importando datos

#%%

###############################################
# Series a niveles (precios)
###############################################
import pandas as pd

Obs = pd.DataFrame(data.High) # se toman conjunto de datos
#%%
Obs2 = Obs.dropna(1)# elimina missinvalues
Series =  Obs2.iloc[:86,:] # longitud
#%%
import matplotlib.pyplot as plt
##########################
# Series Visualización
##########################
plt.style.use('ggplot')
Series.plot(legend  = None)
plt.title('Precio Activos 2017-04-20 a 2018-10-20')
plt.show()




#%%
import numpy as np
##############################################################################
# Creando cambio estructurales (cambios en las medias)
##############################################################################
Series.iloc[:10,:] = Series.iloc[:10,:] + .1
Series.iloc[10:20,:]  = Series.iloc[10:20,:] + .5
Series.iloc[20:50,:] = Series.iloc[20:50,:] +1
Series.iloc[50:,:] = Series.iloc[50:,:] -.5
#%%
# Retornos
Series.iloc[:,:] = np.diff(np.log( Series.iloc[:,:]),0)
#%%
####################################################
# Indicadora del ciclo (grupo)
####################################################
a1 = np.repeat(1,len(Series.iloc[:10,:]))
a2 = np.repeat(2,len(Series.iloc[10:20,:]))
a3 = np.repeat(3,len(Series.iloc[20:50,:]))
a4 = np.repeat(4,len(Series.iloc[50:,:]))
indice = np.hstack((a1,a2,a3,a4))

Series = pd.concat([Series.reset_index(drop=True),pd.DataFrame(indice.tolist(), columns=['y'])], axis=1)


#%%
####################################################
# Test MANOVA (diferentes estadísticos)
####################################################
import pandas as pd
from statsmodels.multivariate.manova import MANOVA
maov = MANOVA.from_formula('AA+AAL+AAP+AAPL+AB+ABBV+ABC+ABM+ABMD+ABT+ACAD+ACHN+ACIW+ACN+ACOR+ADBE+ADI+ADM+ADP+ADSK+AEE+AEO+AEP+AES+AFL+AG+AGIO+AGN+AIG+AINV+AIV+AKAM+AKS+ALK+ALL+ALNY+AMAT+AMD+AMGN+AMP+AMTD+AMZN+AN+ANTM+APA+APC+ARCC+ARLP+ARNA+ARR+ASH+ATI+ATVI+AUY+AVB+AVP+AVXL+AVY+AWK+AXP+AZN+BABA+BAC+BDX+BUD+CS+DAL+DD+FNMA+GOOG+GOOGL+LH+LLY+LUV+MO+MT+NAT+NLY+NVO+PAA+T+UA+UBS+WBA~ y', data=Series)
#%%
##############################################################################
# resultado manova Ho igualdad en as medias dado covarianzas
##############################################################################
print(maov.mv_test())
#%%
##########################
# Test Traicy-Widom
##########################

##########################
# Distribución TW F1
##########################
f90 = t_1[F1>=.90].min()
f95 = t_1[F1>=.95].min()
f99 = t_1[F1>=.99].min()
f05 = t_1[F1>=(1-.05)].min()

##########################
# Parametros
##########################
s= 84
m2 =-1/2
n2 = -1/2
N = 2*(84+84+84)+1

##########################
# Fromula
##########################
gamma = 2*np.arcsin(np.sqrt((84-.5)/N))
phi = 2*np.arcsin(np.sqrt((84-.5)/N))
mu =2*np.log(np.tan((phi+gamma)/2))
sigma3 = (16/N**2)*(1/((np.sin(phi+gamma)**2)*np.sin(phi)*np.sin(gamma)))
sigma = sigma3**(1/3)
theta1 = np.exp(mu + f90*sigma)/(1+np.exp(mu + f90*sigma))
theta2 = np.exp(mu + f95*sigma)/(1+np.exp(mu + f95*sigma))
theta3 = np.exp(mu + f99*sigma)/(1+np.exp(mu + f99*sigma))
theta0 = np.exp(mu + f05*sigma)/(1+np.exp(mu + f05*sigma))

####################################################
# Valor crítico Traicy Widom
####################################################
print(theta0)

print("pvalue", p1-F1[t_1>theta0][-1])
#%%
##############################################################################################
##############################################################################################
##############################################################################################
########################  Ejercicio 3 . freeness #############################################
##############################################################################################
##############################################################################################
##############################################################################################



##################
# Simbolico
##################

from sympy import symbols
from sympy import solve
p = [0.3, 0.5, 0.7]
n = 100
t = 1000
T = 150 # rectangular
c = n/T
alpha = (1-c)/c

#%%

##################
# Monte Carlo
##################

######################################################
# Funcion simulacion:
# input : 
#           p : valor entre 0 a 1
#           n : dimensión GOE
#           t : iteraciones
#           T : dimensión columna Wishart
# output : 
#           simulacion : valores propios simulacion
######################################################


def simulacion(p,n,t, T):
    valores = []

    if n >= T:
        print("Error: N debe ser mayor que Y")

    for i in range(t):
        M = np.random.normal(0,1 ,(n,n)) # GOE
        M = (M + M.T )/2
    
        H = np.random.normal(0,1 ,(n,T)) # Wishart
        W = H@H.T
    
        H = (p*M)/np.sqrt(n) + ((1-p)*W)/n # GOE + Wishart
    
        v, w = np.linalg.eigh(H) # valores y vectores propios
    
        valores.append(v)
        simulacion = np.asarray(valores).flatten()
        
    return simulacion

#%%
##################
# Simulación
##################

sim1 = simulacion(p[0], n, t, T)
sim2 = simulacion(p[1], n, t, T)
sim3 = simulacion(p[2], n, t, T)

#%%

##################
# Visualización
##################

sns.distplot(sim3, hist = False, kde = True,label="p= 0.7")
sns.distplot(sim1, hist = False, kde = True,label="p= 0.3")
sns.distplot(sim2, hist = False, kde = True,label="p= 0.5")
plt.ylabel("p(x)")
plt.xlabel("x")
plt.title("Simulación")
plt.savefig('Ej3_fig1.png')

#%%

##################
# Simbólica
##################

G, z, a , w  = symbols('G z a w') # Simbolos
Solve = solve((w**2)*G/2 + ((1-w)*(1+a))/(1-(1-w)*G)+ (1/G) -z, G) # Ecuación


#%%

##################
# Solución G
##################

# [-(-3*(-2*a*w + 2*a - 2*z)/(w**3 - w**2) + (w**2 - 2*w*z + 2*z)**2/(w**3 - w**2)**2)/(3*(sqrt(-4*(-3*(-2*a*w + 2*a - 2*z)/(w**3 - w**2) + (w**2 - 2*w*z + 2*z)**2/(w**3 - w**2)**2)**3 + (54/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(w**3 - w**2)**2 + 2*(w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**2)/2 + 27/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(2*(w**3 - w**2)**2) + (w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**(1/3)) - (sqrt(-4*(-3*(-2*a*w + 2*a - 2*z)/(w**3 - w**2) + (w**2 - 2*w*z + 2*z)**2/(w**3 - w**2)**2)**3 + (54/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(w**3 - w**2)**2 + 2*(w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**2)/2 + 27/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(2*(w**3 - w**2)**2) + (w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**(1/3)/3 - (w**2 - 2*w*z + 2*z)/(3*(w**3 - w**2)),
#  -(-3*(-2*a*w + 2*a - 2*z)/(w**3 - w**2) + (w**2 - 2*w*z + 2*z)**2/(w**3 - w**2)**2)/(3*(-1/2 - sqrt(3)*I/2)*(sqrt(-4*(-3*(-2*a*w + 2*a - 2*z)/(w**3 - w**2) + (w**2 - 2*w*z + 2*z)**2/(w**3 - w**2)**2)**3 + (54/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(w**3 - w**2)**2 + 2*(w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**2)/2 + 27/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(2*(w**3 - w**2)**2) + (w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**(1/3)) - (-1/2 - sqrt(3)*I/2)*(sqrt(-4*(-3*(-2*a*w + 2*a - 2*z)/(w**3 - w**2) + (w**2 - 2*w*z + 2*z)**2/(w**3 - w**2)**2)**3 + (54/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(w**3 - w**2)**2 + 2*(w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**2)/2 + 27/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(2*(w**3 - w**2)**2) + (w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**(1/3)/3 - (w**2 - 2*w*z + 2*z)/(3*(w**3 - w**2)),
#  -(-3*(-2*a*w + 2*a - 2*z)/(w**3 - w**2) + (w**2 - 2*w*z + 2*z)**2/(w**3 - w**2)**2)/(3*(-1/2 + sqrt(3)*I/2)*(sqrt(-4*(-3*(-2*a*w + 2*a - 2*z)/(w**3 - w**2) + (w**2 - 2*w*z + 2*z)**2/(w**3 - w**2)**2)**3 + (54/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(w**3 - w**2)**2 + 2*(w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**2)/2 + 27/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(2*(w**3 - w**2)**2) + (w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**(1/3)) - (-1/2 + sqrt(3)*I/2)*(sqrt(-4*(-3*(-2*a*w + 2*a - 2*z)/(w**3 - w**2) + (w**2 - 2*w*z + 2*z)**2/(w**3 - w**2)**2)**3 + (54/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(w**3 - w**2)**2 + 2*(w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**2)/2 + 27/(w**3 - w**2) - 9*(w**2 - 2*w*z + 2*z)*(-2*a*w + 2*a - 2*z)/(2*(w**3 - w**2)**2) + (w**2 - 2*w*z + 2*z)**3/(w**3 - w**2)**3)**(1/3)/3 - (w**2 - 2*w*z + 2*z)/(3*(w**3 - w**2))]

#%%

###################
# Solución p = 0.3
###################

Npts = 100
x = np.linspace(np.min(sim1), np.max(sim1), Npts)

solution = [(Solve[i]).subs({a:alpha, w:p[0]}) for i in range(3)] # sustituye alpha y p

G = [] 

for i in range(Npts):
   
    # sustituye z con x's
    tmp1 = (solution[0]).subs({z:x[i]})
    tmp2 = (solution[1]).subs({z:x[i]})
    tmp3 = (solution[2]).subs({z:x[i]})
    
    # restricción
    if im(tmp1) == 0:
        G.append(np.abs(im(tmp2))/np.pi)
    else:
        G.append(np.abs(im(tmp1))/np.pi)
        


#%%

###################
# Solución p = 0.5
###################
        
x2 = np.linspace(np.min(sim2), np.max(sim2), Npts)
solution2 = [(Solve[i]).subs({a:alpha, w:p[1]}) for i in range(3)] # sustituye alpha y p
G2 = []

for i in range(Npts):
   
    # sustituye z con x's
    tmp1 = (solution2[0]).subs({z:x2[i]})
    tmp2 = (solution2[1]).subs({z:x2[i]})
    tmp3 = (solution2[2]).subs({z:x2[i]})

    # restricción
    if im(tmp1) == 0:
        G2.append(np.abs(im(tmp2))/np.pi)
    else:
        G2.append(np.abs(im(tmp1))/np.pi)

#%%

###################
# Solución p = 0.7
###################
        
x3 = np.linspace(np.min(sim3), np.max(sim3), Npts)
solution3 = [(Solve[i]).subs({a:alpha, w:p[2]}) for i in range(3)] # sustituye alpha y p
G3 = []

for i in range(Npts):
   
    # restricción
    tmp1 = (solution3[0]).subs({z:x3[i]})
    tmp2 = (solution3[1]).subs({z:x3[i]})
    tmp3 = (solution3[2]).subs({z:x3[i]})

    # restricción
    if im(tmp1) == 0:
        G3.append(np.abs(im(tmp2))/np.pi)
    else:
        G3.append(np.abs(im(tmp1))/np.pi)

#%%     
###################
# Plot p = 0.3
###################
        
sns.distplot(sim1, hist = False, kde = True, label="p= 0.3")
plt.plot(x, G, "--", label="p= 0.3")
plt.ylabel("p(x)")
plt.xlabel("x")
plt.legend()


plt.savefig('Ej3_fig2.png')
#%%
###################
# Plot p = 0.5
###################
sns.distplot(sim2, hist = False, kde = True, label="p= 0.5")
plt.plot(x2, G2, "--", label="p= 0.5")
plt.ylabel("p(x)")
plt.xlabel("x")
plt.legend()

plt.savefig('Ej3_fig3.png')
#%%
###################
# Plot p = 0.7
###################
sns.distplot(sim3, hist = False, kde = True, label="p= 0.7")
plt.plot(x3, G3, "--", label="p= 0.7")
plt.ylabel("p(x)")
plt.xlabel("x")
plt.legend()

plt.savefig('Ej3_fig4.png')
#%%
###################
# Visualización
###################

# p = 0.3
sns.distplot(sim1, hist = False, kde = True, label="p= 0.3", color = "black")
plt.plot(x, G, "--", label="p= 0.3", color = "black")

# p = 0.5
sns.distplot(sim2, hist = False, kde = True, label="p= 0.5", color = "r")
plt.plot(x2, G2, "--", label="p= 0.5", color = "r")

# p = 0.7
sns.distplot(sim3, hist = False, kde = True, label="p= 0.7", color = "b")
plt.plot(x3, G3, "--", label="p= 0.7", color = "b")
plt.ylabel("p(x)")
plt.xlabel("x")
plt.legend()

plt.savefig('Ej3_fig5.png')