##############################################################################################
##############################################################################################
##############################################################################################
########################  Ejercicio 1 . Frontera optima ######################################
##############################################################################################
##############################################################################################
##############################################################################################

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.pyplot as plt
import seaborn as sns
path = 'C:\\Users\\h_air\\Documents\\Semestre 3\\Tópicos Selectos en Estadística\\Matrices aleatorias\\Tareas\\Tarea 5'
os.chdir(path)
###############################################
# Funciones de Riesgo
###############################################

# Riesgo verdadero
Rtrue = lambda g, sigma, mu: g**2 / (mu.T@np.linalg.inv(sigma)@mu).flatten()
# Riesgo dentro de la muestra (in-sample)
Rin =  lambda g, s, mu: g**2 / (mu.T@np.linalg.inv(s)@mu).flatten()
# Riesgo fuera de la muestra (out-sample)
Rout = lambda g,  s, sigma, mu: (g**2)*(mu.T@np.linalg.inv(s)@sigma@np.linalg.inv(s)@mu) / ((mu.T@np.linalg.inv(s)@mu)**2).flatten()

#%%
###############################################
# Variable
###############################################


def frontera_opt(n, p, var, mu):
    
    sigma = np.eye(p) # Matriz de covarianza de los retornos de los activos


    # Matriz de covarianza muestra como ensamble de Wishart

    X =  np.random.normal(0,1, (p, n)) # Matriz aleatoria de Wishart
    D = np.diag(np.repeat(var, p)) # Factor que hace varianza de .2
    z = D@X # con varianza de .2
    s = np.corrcoef(z) # Mátriz de Covarianza muestral S
    g = np.linspace(1,100,100) # Ganancia esperada del portafolio
    # Riesgos
    R_true = Rtrue(g,sigma,mu) # True
    R_in = Rin(g,s,mu) # In-sample
    R_out = Rout(g,s, sigma, mu) # Out-sample
    plt.style.use('ggplot')
    plt.plot(R_in.flatten(), g, label ="In", c ="orange")
    plt.plot(R_true.flatten(), g, label ="True",  c ="b")
    plt.plot(R_out.flatten(), g, label ="out",  c ="g")
    plt.title('Frontera Optima')
    plt.legend()
#%%

n = 200 # Longitud Activos
p = 100 # Activos
var = .2 # Whishart ensamble varianza
mu = np.ones((p,1)) # Retorno esperado

frontera_opt(n, p, var, mu)
plt.savefig('Ej1_fig1.png')

#%%

###############################################
# Pregunta 1
###############################################

## Caso 1 n constante
# p < n 
n = 200 # Longitud Activos
p = 50 # Activos
var = .2 # Whishart ensamble varianza
mu = np.ones((p,1)) # Retorno esperado

frontera_opt(n, p, var, mu)
plt.savefig('Ej1_fig2.png')

#%%

# p = n 
n = 200 # Longitud Activos
p = 200 # Activos
var = .2 # Whishart ensamble varianza
mu = np.ones((p,1)) # Retorno esperado

frontera_opt(n, p, var, mu)
plt.savefig('Ej1_fig3.png')

#%%

## Caso 2 p constante
# p << n 
n = 1000 # Longitud Activos
p = 100 # Activos
var = .2 # Whishart ensamble varianza
mu = np.ones((p,1)) # Retorno esperado

frontera_opt(n, p, var, mu)

plt.savefig('Ej1_fig4.png')
#%%

## Caso 3 p y n tienden al infinito, tal que c es constante
# p << n 
n = 1000 # Longitud Activos
p = 900 # Activos
var = .2 # Whishart ensamble varianza
mu = np.ones((p,1)) # Retorno esperado

frontera_opt(n, p, var, mu)

# existe mucho ruido tal que se abre
plt.savefig('Ej1_fig5.png')


#%%

###############################################
# Pregunta 2
###############################################

# Caso 1 Varianza menor a .2, p y n iniciales

n = 200 # Longitud Activos
p = 100 # Activos
var = .02 # Whishart ensamble varianza
mu = np.ones((p,1)) # Retorno esperado

frontera_opt(n, p, var, mu)
plt.savefig('Ej1_fig6.png')

#%%
# Caso 2 Varianza mayor a .2, p y n iniciales

n = 200 # Longitud Activos
p = 100 # Activos
var = 2 # Whishart ensamble varianza
mu = np.ones((p,1)) # Retorno esperado

frontera_opt(n, p, var, mu)

plt.savefig('Ej1_fig7.png')
#%%
# Caso 3 Varianza mucho mayor a .2, p y n iniciales

n = 200 # Longitud Activos
p = 100 # Activos
var = 20 # Whishart ensamble varianza
mu = np.ones((p,1)) # Retorno esperado

frontera_opt(n, p, var, mu)
plt.savefig('Ej1_fig8.png')

#%%
###############################################
# Pregunta 3
###############################################

# Caso 1  media varia del 1 al 10

n = 200 # Longitud Activos
p = 100 # Activos
var = .02 # Whishart ensamble varianza
mu = np.random.randint(1, 10, p) # Retorno esperado

frontera_opt(n, p, var, mu)

plt.savefig('Ej1_fig9.png')

#%%
# Caso 2  media varia del 1 al 100

n = 200 # Longitud Activos
p = 100 # Activos
var = .02 # Whishart ensamble varianza
mu = np.random.randint(1, 100, p) # Retorno esperado

frontera_opt(n, p, var, mu)
plt.savefig('Ej1_fig10.png')


#%%

##############################################################################################
##############################################################################################
##############################################################################################
########################  Ejercicio 2. Método Laloux et.al ###################################
##############################################################################################
##############################################################################################
##############################################################################################

###############################################
# Series financieras Yahoo finances
###############################################

# Activos
Stock_Prices = ['AMD','AAPL','BAC','AMZN','T','GOOG','MO','DAL','AA','AXP','DD','BABA','ABT',
'UA','AMAT','AMGN','AAL','AIG','ALL','ADBE','GOOGL','ACN','ABBV','MT','LLY','AGN','APA','ADP',
'APC','AKAM','NLY','AEP','ATVI','ADSK','ADM','BMH.AX','WBA','ARNA','LUV','ACAD']

# Descarga series
import yfinance as yf
data = yf.download(Stock_Prices,"2017-04-20","2018-12-30")

#%%

###############################################
# Series a niveles (precios)
###############################################


Obs = pd.DataFrame(data.Close)
Obs = Obs.dropna() # elimina missinvalues
Series =  Obs.iloc[:401,:] # longitud

#Series = Series.as_matrix()
# Series.isna().sum()

# Series
plt.style.use('ggplot')
Series.plot(legend  = None)
plt.plot()
plt.title('Precio Activos 2017-04-20 a 2018-12-30')
plt.show()

plt.savefig('Ej2_fig1.png')



#%%

###############################################
# Series a primeras diferencias (retornos)
###############################################
# Retornos de la serie
Log_precios = np.log(Series)
#Retornos = np.diff(Log_precios, axis = 0)
Retornos = (np.diff(Series, axis = 0)/Series.iloc[:-1,:])
# Series
plt.style.use('ggplot')
plt.plot(Retornos) # primeras diferencias
plt.title('Retornos de los activos')
plt.show()
plt.savefig('Ej2_fig2.png')

#%%

###############################################
# Metodología del paper
###############################################

R1 = Retornos.iloc[:200, :].as_matrix()
R2 = Retornos.iloc[200:, :].as_matrix()

#%%

###############################################
# Primera mitad de los datos
###############################################

C1 = np.corrcoef(np.transpose(R1))
mu1 =R1.mean(0).reshape(40,1)

v1, o1 = np.linalg.eigh(C1)
v_sort1 = np.sort(v1)[::-1]
o_sort1 = o1[np.argsort(v1)[::-1]]

#%%

###############################################
# Marcenko Pastur
###############################################
p = 40
n = 400    
c = p/n
maximo = (1 + np.sqrt(c))**2
minimo = (1 - np.sqrt(c))**2
lam = np.linspace(minimo,maximo , 100)

Marcenko_Pastur =  np.sqrt((maximo - lam)*(lam - minimo))/ (2*c*np.pi*lam) 
plt.style.use('ggplot')
plt.plot(lam, Marcenko_Pastur , c = 'black')
plt.fill_between(lam, 0, Marcenko_Pastur,  color = 'black')

plt.axhline(y=0,  color='red', c = 'black')
plt.axvline(x=0,  color='red', c = 'black')
plt.title('Marcenko_Pastur')
#plt.savefig('Ej2_fig3.png')

#%%

###############################################
#  Marcenko Pastur y valore propios Retornos
###############################################
plt.style.use('ggplot')
plt.plot(lam, Marcenko_Pastur , c = 'black')
plt.fill_between(lam, 0, Marcenko_Pastur,  color = 'black')
for i in range(len(v_sort1)):
    plt.axvline(x=v_sort1[i], ymin = 0, ymax = .2, color='red', ls = '--')
plt.axhline(y=0,  color='red', c = 'black')
plt.axvline(x=0,  color='red', c = 'black')


plt.title('Marcenko_Pastur and Eigenvalues Returns')
plt.savefig('Ej2_fig4.png')
#%%

###############################################
#  Marcenko Pastur y valore propios Retornos
###############################################

plt.style.use('ggplot')   
for i in range(len(v_sort1[v_sort1>maximo])):
    plt.axvline(x=v_sort1[v_sort1>maximo][i], ymax = .2, color='red', ls = '--')
    
plt.plot(lam, Marcenko_Pastur , c = 'black')
plt.fill_between(lam, 0, Marcenko_Pastur,  color = 'black')
plt.axhline(y=0,  color='red', c = 'black')
plt.axvline(x=0,  color='red', c = 'black')

           
plt.title('Marcenko_Pastur and Eigenvalues Returns')
plt.savefig('Ej2_fig5.png')
#%%

###############################################
#  Risk in sample (clean and dirty)
###############################################

v_grandes1 =  v_sort1[v_sort1>maximo]
temp1 = v_sort1[v_sort1>maximo].shape[0]
temp_2_1 = np.repeat(1, 40-temp1)
v_clean1 = np.diag(np.concatenate((v_grandes1, temp_2_1)))
C_hat1 = o_sort1@v_clean1@o_sort1.T
print('traza sigma original', np.trace(C1),
      'traza sigma limpia', np.trace(C_hat1))

g = np.linspace(1,100,100) # Ganancia esperada del portafolio

Rin =  lambda g, s, mu: g**2 / (mu.T@np.linalg.inv(s)@mu).flatten()


R_in_dirty = Rin(g,C1,mu1) # In-sample
R_in_clean = Rin(g,C_hat1,mu1) # In-sample
#R_out = Rout(g,s, sigma, mu) # Out-sample

plt.style.use('ggplot')
plt.plot(R_in_clean.flatten(), g,   c ="g", label = "Clean(In)") # clean
plt.plot(R_in_dirty.flatten(), g,   c ="b", label = "Dirty(In)") # dirty

plt.title('Frontera Optima In-sample Risk')
plt.legend()

plt.savefig('Ej2_fig6.png')

#%%

###############################################
#  Segunda mitad de los datos
###############################################

C2 = np.corrcoef(np.transpose(R2))
mu2 =R2.mean(0).reshape(40,1)
mu =  Retornos.mean(0).as_matrix().reshape(40,1)
v2, o2 = np.linalg.eigh(C2)
v_sort2 = np.sort(v2)[::-1]
o_sort2 = o2[np.argsort(v2)[::-1]]




#%%

###############################################
#  Marcenko Pastur y valore propios Retornos
###############################################

plt.style.use('ggplot')
plt.plot(lam, Marcenko_Pastur , c = 'black')
plt.fill_between(lam, 0, Marcenko_Pastur,  color = 'black')
for i in range(len(v_sort2)):
    plt.axvline(x=v_sort2[i], ymin = 0, ymax = .2, color='red', ls = '--')
plt.axhline(y=0,  color='red', c = 'black')
plt.axvline(x=0,  color='red', c = 'black')
plt.title('Marcenko Pastur')
plt.legend()
plt.savefig('Ej2_fig7.png')
#%%

###############################################
#  Marcenko Pastur y valore propios Retornos
###############################################

for i in range(len(v_sort2[v_sort2>maximo])):
    plt.axvline(x=v_sort2[v_sort2>maximo][i], ymax = .2, color='red', ls = '--')
plt.style.use('ggplot')    
plt.plot(lam, Marcenko_Pastur , c = 'black')
plt.fill_between(lam, 0, Marcenko_Pastur,  color = 'black')
plt.axhline(y=0,  color='red', c = 'black')
plt.axvline(x=0,  color='red', c = 'black')
plt.title('Marcenko Pastur')
plt.legend()
         
plt.savefig('Ej2_fig8.png')  
#%%
###############################################
#  Risk out sample (clean and dirty)
###############################################

v_grandes2 =  v_sort2[v_sort2>maximo]
temp2 = v_sort2[v_sort2>maximo].shape[0]
temp_2_2 = np.repeat(.8, 40-temp2)
v_clean2 = np.diag(np.concatenate((v_grandes2, temp_2_2)))
C_hat2 = o_sort1@v_clean2@o_sort1.T
print('traza sigma original', np.trace(C2),
      'traza sigma limpia', np.trace(C_hat2))

Rout = lambda g,  s, sigma, mu: (g**2)*(mu.T@np.linalg.inv(s)@sigma@np.linalg.inv(s)@mu) / ((mu.T@np.linalg.inv(s)@mu)**2).flatten()


g = np.linspace(1,100,100) # Ganancia esperada del portafolio
R_out_dirty = Rout(g,  C1, C2, mu2) # In-sample
R_out_clean = Rout(g, C_hat1, C_hat2, mu2) # In-sample
#R_out = Rout(g,s, sigma, mu) # Out-sample

plt.style.use('ggplot')
plt.plot(R_out_clean.flatten(), g,   c ="g", label = "Clean(out)") # clean
plt.plot(R_out_dirty.flatten(), g,   c ="b", label = "Dirty(out)") # dirty

plt.title('Frontera Optima Out-sample Risk')
plt.legend()

plt.savefig('Ej2_fig9.png')
#%%

###############################################
#  Figura 2 paper
###############################################
plt.style.use('ggplot')
plt.plot(R_in_dirty.flatten(), g,   c ="r", label = "Dirty(In)") # dirty
plt.plot(R_out_dirty.flatten(), g,   c ="g", label = "Dirty(Out)") # dirty


plt.plot(R_in_clean.flatten(), g,   c ="r", ls ='--', label = "Clean(In)") # clean
plt.plot(R_out_clean.flatten(), g,   c ="g", ls ='--' , label = "Clean(Out)") # clean

plt.title('Frontera Optima')
plt.legend()
plt.savefig('Ej2_fig10.png')

#%%

##############################################################################################
##############################################################################################
##############################################################################################
########################  Ejercicio 3. Distribución tracy-Widom ##############################
##############################################################################################
##############################################################################################
##############################################################################################

###############################################
#  Tracy-Widom Distribución beta = 2
###############################################

import numpy as np
from scipy import integrate
from scipy.special import airy
import matplotlib.pyplot as plt

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
plt.plot(t,F1, "g", label = "beta=1")
plt.plot(t,F2,"b", label = "beta=2")
plt.ylabel('f(s)')
plt.xlabel('s')
plt.title('Distribución Tracy-Widom')
plt.legend()

#plt.savefig('Ej3_fig1.png')



#%%

#########################
# Simulación y T-W F_2
#########################
import numpy as np
n = 100
t = 5000
valores = []
dx = .2

for i in range(t):
    a = np.matrix( np.random.normal(0,1 ,(n,n)) -1j*np.random.normal(0,1 ,(n,n)))
    s = (a + a.getH() )/2
    v, w = np.linalg.eigh(s)
    v_max = np.max(v)
    valores.append(v_max)

    
#%%  
V = np.array(valores)
V2 = (n**(1/6))*(V-2*np.sqrt(n))
#%%

plt.style.use('ggplot')
plt.plot(t,f2,"b", label = "beta=2")
plt.hist(V2, 50, density = True, color ="black", label = "simulación")
plt.legend()
plt.title('Empirica vs Ley Traicy Widom ')
plt.xlim(-5, 2)
# valor propio más grande de la matriz hermitian

plt.savefig('Ej3_figExtra.png')

#%%
#########################
# Densidad
#########################

plt.plot(t,f1, "g", label = "beta=1")
plt.plot(t,f2,"b", label = "beta=2")
plt.ylabel('f(s)')
plt.xlabel('s')
plt.title('Densidad Tracy-Widom')
plt.legend()


#plt.savefig('Ej3_fig2.png')

#%%

##############################################################################################
##############################################################################################
##############################################################################################
########################  Ejercicio 4. #######################################################
##############################################################################################
##############################################################################################
##############################################################################################

# poner en describción del problema
p = 10
n = 10    
c = p/n
maximo = (1 + np.sqrt(c))**2
minimo = (1 - np.sqrt(c))**2
lam = np.linspace(minimo,maximo , 100)

Marcenko_Pastur =  np.sqrt((maximo - lam)*(lam - minimo))/ (2*c*np.pi*lam) 
plt.style.use('ggplot')
plt.plot(lam, Marcenko_Pastur , c = 'black')

plt.axvline(x=4.2, ymin = 0, ymax = .3, color='black', ls = '--')
plt.fill_between(lam, 0, Marcenko_Pastur,  color = 'black')
plt.title('Marcenko  Pastur')
plt.ylabel('f(x)')
plt.xlabel('x')
plt.savefig('Ej4_figExtra.png')

# para ver si eso se cumple mejor revisa en la de densida de valor mas grande
#%%
lam =4.2
n=10
p=10

#########################
# Primer Orden
#########################
mu_np = (np.sqrt(n)+np.sqrt(p))**2
sigma_np = (mu_np)*( (1/np.sqrt(n)) + (1/np.sqrt(p)) )**(1/3)
# Valor crítico
lam_norm = (n*lam - mu_np)/sigma_np

#########################
# Segundo orden
#########################
mu_np0 =(np.sqrt(n-.5)+np.sqrt(p-.5))**2
sigma_np0 =(np.sqrt(n-.5)+np.sqrt(p-.5))*((1/(np.sqrt(n-.5)))+(1/(np.sqrt(p-.5))))**(1/3)

# valor crítico
lam_norm0 = (n*lam - mu_np0)/sigma_np0

#%%
plt.plot(t, f1, color = "black")
plt.fill_between(t[t>lam_norm],0, f1[t>lam_norm], color = "red")
plt.axvline(x=lam_norm, ymin = 0, ymax = .5, color='black', ls = '--')
plt.title('Aproximación primer orden')
plt.ylabel('f(s)')
plt.xlabel('s')
#plt.savefig('Ej4_fig1.png')

#%%
plt.plot(t, f1, color = "black")
plt.fill_between(t[t>lam_norm0],0, f1[t>lam_norm0], color = "red")
plt.axvline(x=lam_norm0, ymin = 0, ymax = .3, color='black', ls = '--')
plt.title('Aproximación segundo orden')
plt.ylabel('f(s)')
plt.xlabel('s')
plt.savefig('Ej4_fig2.png')

#%%


alpha = [1/100, 5/100, 10/100]

for i in alpha:    
    if lam_norm<0:
        print("p-valor:", F1[t>lam_norm][-1])
        critico = F1[t>lam_norm][-1]
        print("Con alpha de:", i, "¿El valor crítico es mayor a alpha?:", critico > i)
    else:
        print("p-valor:", 1-F1[t>lam_norm][-1])
        critico = 1-F1[t>lam_norm][-1]
        print("Con alpha de:", i, "¿El valor crítico es mayor a alpha?:", critico > i)
        
#%%
alpha = [1/100, 5/100, 10/100]

for i in alpha:    
    if lam_norm0<0:
        print("p-valor:", F1[t>lam_norm0][-1])
        critico0 = F1[t>lam_norm0][-1]
        print("Con alpha de:", i, "¿El valor crítico es mayor a alpha?:", critico0 > i)
    else:
        print("p-valor:", 1-F1[t>lam_norm0][-1])
        critico0 = 1-F1[t>lam_norm0][-1]
        print("Con alpha de:", i, "¿El valor crítico es mayor a alpha?:", critico0 > i)

#%%





##############################################################################################
##############################################################################################
##############################################################################################
########################  Fin Tarea 5. #######################################################
##############################################################################################
##############################################################################################
##############################################################################################