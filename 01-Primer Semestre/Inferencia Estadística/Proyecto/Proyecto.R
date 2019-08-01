#PROYECTO INFERENCIA
#Datos
Datos <- datasets::austres
Numbers (in thousands) of Australian residents measured quarterly from March 1971 to March 1994.
P. J. Brockwell and R. A. Davis. Introduction to Time Series and Forecasting. Springer-Verlag, 1996. ISBN
978-1-4757-2526-1. [p228]
#plot de la serie 
head(economics)
library(ggplot2)
library(xts)
library(astsa)
autoplot(Datos, xlab = "Tiempo", ylab  = "Residentes en Australia")

library(ggfortify)
autoplot(stl(Datos, s.window = 'periodic'), ts.colour = 'blue')
#autoplot(acf(Datos, plot = FALSE))

# Funcion del filtro de kalman 
# Missing Value Imputation by Kalman Smoothing and State Space
#Models
new_imp <- function(In){
  out <- imputeTS::na.kalman(In, "auto.arima")
  out <- as.numeric(out)
  return(as.ts(out))
}
#For using the state space representation of arima model (using
#auto.arima)
#The KalmanSmoother used in this function is KalmanSmooth
#na.kalman(x, model = "StructTS", smooth = TRUE, nit = -1, ...)
#Uses Kalman Smoothing on structural time series models 
#(or on the state space representation of an
#arima model) for imputation.

sample_dat(Datos, smps = "mcar", repetition = 20, b = 10, blck = 50,
           blckper = TRUE, plot = TRUE)
sample_dat(Datos, smps = "mcar", repetition = 20, b = 20, blck = 50,
           blckper = TRUE, plot = TRUE)
sample_dat(Datos, smps = "mcar", repetition = 20, b = 80, blck = 50,
           blckper = TRUE, plot = TRUE)
sample_dat(Datos, smps = "mcar", repetition = 20, b = 90, blck = 50,
           blckper = TRUE, plot = TRUE)
b - Numeric indicating the total amount of missing data as a percentage to remove from the complete
time series

mar- block -The block size
can be specified as a percentage of the total amount of missing observations to remove or as a number
of time steps in the input dataset.

The MCAR sampling scheme assumes all observations have equal probability of
being selected for removal and is appropriate for understanding imputation accuracy with univariate
time series that are not serially correlated

MAR sampling scheme
selects observations in blocks such that the probability of selection for a single observation depends on
whether an observation closer in time was also selected (Schafer and Graham, 2002). The MAR scheme
is appropriate for time series with serial correlation

This dataset includes a simple correlation structure with minimal random noise
#imputando errir 
library(ggplot2)
p<- plot_impute(dataIn = Datos, showmiss = T,
            smps = "mcar", methods = c("kalman","na.approx", "na.locf", "na.mean"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
            missPercent = 40) 
p + scale_colour_manual(values = c('black', 'grey'))+ ggtitle('Imputación 40% Valores perdidos') + scale_y_continuous('Población Australia')

p2<- plot_impute(dataIn = Datos, showmiss = T,
                smps = "mcar", methods = c("kalman","na.approx",  "na.locf", "na.mean"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
                missPercent = 80) 
p2 + scale_colour_manual(values = c('black', 'grey'))+ ggtitle('Imputación 80% Valores perdidos') + scale_y_continuous('Población Australia')


########################################
p<- plot_impute(dataIn = Datos, showmiss = T,
                smps = "mcar", methods = c("kalman","na.approx", "na.interp",
                                           "na.interpolation"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
                missPercent = 10) 
p + scale_colour_manual(values = c('black', 'grey'))+ ggtitle('Imputación 10% Valores perdidos') + scale_y_continuous('Población Australia')

p2<- plot_impute(dataIn = Datos, showmiss = T,
                 smps = "mcar", methods = c("kalman","na.approx", "na.interp",
                                            "na.interpolation"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
                 missPercent = 40) 
p2 + scale_colour_manual(values = c('black', 'grey'))+ ggtitle('Imputación 40% Valores perdidos') + scale_y_continuous('Población Australia')


p3<- plot_impute(dataIn = Datos, showmiss = T,
                 smps = "mcar", methods = c("kalman","na.approx", "na.interp",
                                            "na.interpolation"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
                 missPercent = 80) 
p3 + scale_colour_manual(values = c('black', 'grey'))+ ggtitle('Imputación 80% Valores perdidos') + scale_y_continuous('Población Australia')

# por la forma en que realiza la imputación




library(imputeTS)
#rmse
error_impute <- impute_errors(Datos, smps = "mcar", methods = c("kalman","na.approx", "na.locf", "na.mean"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
              errorParameter = "rmse", errorPath = NULL, blck = 50, blckper = TRUE,
              missPercentFrom = 10, missPercentTo = 90, interval = 10,
              repetition = 10, addl_arg = NULL)

plot_errors(error_impute, plotType = c("boxplot"))
plot_errors(error_impute, plotType = c("bar"))
plot_errors(error_impute, plotType = c("line")) # de impute error
error_impute2 <- impute_errors(Datos, smps = "mcar", methods = c("kalman","na.approx", "na.interp",
                                                                "na.interpolation"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
                              errorParameter = "rmse", errorPath = NULL, blck = 50, blckper = TRUE,
                              missPercentFrom = 10, missPercentTo = 90, interval = 10,
                              repetition = 10, addl_arg = NULL)
plot_errors(error_impute2, plotType = c("boxplot"))
plot_errors(error_impute2, plotType = c("bar"))
plot_errors(error_impute2, plotType = c("line")) # de impute error

#mape
error_impute <- impute_errors(Datos, smps = "mcar", methods = c("kalman","na.approx",  "na.locf", "na.mean"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
                              errorParameter = "mape", errorPath = NULL, blck = 50, blckper = TRUE,
                              missPercentFrom = 10, missPercentTo = 90, interval = 10,
                              repetition = 10, addl_arg = NULL)

plot_errors(error_impute, plotType = c("boxplot"))
plot_errors(error_impute, plotType = c("bar"))
plot_errors(error_impute, plotType = c("line")) # de impute error
error_impute2 <- impute_errors(Datos, smps = "mcar", methods = c("new_imp","na.approx", "na.interp",
                                                                 "na.interpolation"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
                               errorParameter = "mape", errorPath = NULL, blck = 50, blckper = TRUE,
                               missPercentFrom = 10, missPercentTo = 90, interval = 10,
                               repetition = 10, addl_arg = NULL)
plot_errors(error_impute2, plotType = c("boxplot"))
plot_errors(error_impute2, plotType = c("bar"))
plot_errors(error_impute2, plotType = c("line")) # de impute error

#mae
error_impute <- impute_errors(Datos, smps = "mcar", methods = c("kalman","na.approx",  "na.locf", "na.mean"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
                              errorParameter = "mae", errorPath = NULL, blck = 50, blckper = TRUE,
                              missPercentFrom = 10, missPercentTo = 90, interval = 10,
                              repetition = 10, addl_arg = NULL)

plot_errors(error_impute, plotType = c("boxplot"))
plot_errors(error_impute, plotType = c("bar"))
plot_errors(error_impute, plotType = c("line")) # de impute error
error_impute2 <- impute_errors(Datos, smps = "mcar", methods = c("kalman","na.approx", "na.interp",
                                                                 "na.interpolation"), methodPath = "C:/Users/h_air/Desktop/CIMAT MCE/Semestre_1/Inferencia Estadística/Proyecto Final Inferencia/R files/FuncionKalmanFilter.r",
                               errorParameter = "mae", errorPath = NULL, blck = 50, blckper = TRUE,
                               missPercentFrom = 10, missPercentTo = 90, interval = 10,
                               repetition = 10, addl_arg = NULL)
plot_errors(error_impute2, plotType = c("boxplot"))
plot_errors(error_impute2, plotType = c("bar"))
plot_errors(error_impute2, plotType = c("line")) # de impute error
