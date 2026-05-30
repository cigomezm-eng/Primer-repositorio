#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           UNIVERSIDAD NACIONAL DE COLOMBIA
#                   Facultad de Ciencias Económicas | 2026  -  1
#                                   Econometría II 
##
#      Metodología Box-Jenkins para la identificación, estimación y pronóstico de
#                           series de tiempo univariadas
#                                  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Limpiamos el entorono 

rm(list = ls())
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  Instalación de Paquetes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

install.packages("here")

# Trabajar con rutas relativas en R
library(fs)
library(here)

# Paquetes del tidyverse (Para el manejo, manipulación y graficación de datos)
library(readr)
library(dplyr)
library(ggplot2)

install.packages("ggtime")
library(ggtime)

# Paquetes del tidyverts (Para un manejo moderno de series de tiempo en R)
library(tsibble)
install.packages("feasts")
library(feasts)
install.packages("feasts")
library(fable)


# Paquetes adicionales para trabajar con series de tiempo en R

install.packages("tseries")
library(tseries)
install.packages("FinTS")
library(FinTS)
library(lmtest)
library(urca) # Test de raíz unitaria
library(readr)

install.packages("patchwork")

# Para que la función ARIMA que se use por defecto sea la de fable.
ARIMA <- fable::ARIMA

# Descargamos e importamos los paquetes que vayamos a usar con el paquete "pacman"

library(pacman)

# Pacman contiene una función denominada "p_load" que permite al usuario descargar
# un paquete e importarlo si no lo tiene, y si el usuario tiene descargado el 
# paquete, Pacman lo importa automáticamente. Veamoslo. 

pacman::p_load(
  
  forecast,   # Para hacer pronósticos con modelos arima
  lmtest,     # Significancia individual de los coeficientes ARIMA
  urca,       # Prueba de raíz unitaria
  tseries,    # Para estimar modelos de series de tiempo y hacer pruebas de supuestos
  stargazer,  # Para presentar resultados más estéticos
  psych,      # Para hacer estadísticas descriptiva
  seasonal,   # Para desestacionalizar series
  aTSA,       # Para hacer la prueba de efectos ARCH
  astsa,      # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA
  xts,        # Para utilizar objetos xts 
  tidyverse,  # Conjunto de paquetes (incluye dplyr y ggplot2)
  readxl,     # Para leer archivos excel 
  car,        # Para usar la función qqPlot
  mFilter,    # Para aplicar el Filtro Hodrick-Prescott
  quantmod,    
  
  # Paquetes del tidyverts
  
  fable,      # Forma moderna de hacer pronóstiocs en R (se recomienda su uso)  
  tsibble,    # Para poder emplear objetos de series de tiempo tsibble
  feasts      # Provee una colección de herramientas para el análisis de datos de series de tiempo 
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                         METODOLOGÍA BOX-JENKINS                              #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####    Identificación ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Cargar bases de datos en R usando rutas relativas ---

# Fijar la ruta del archivo actual como referencia para here()

here::i_am("Codigo/codigo_metodologiabj_.R") 

# Obtener la ruta del directorio con los datos
directorio <- fs::path(here::here("HQMCB12YR.csv", "Datos"))

# Rutas de las bases de datos
ruta_tasas <- fs::path(directorio, "HQMCB12YR.csv") # Base de datos de Tasas

# Funciones auxiliares ---

# Función auxiliar para mostrar gráficos en una grilla m x n

grilla <- function(..., nrow, ncol) {
  graficos <- list(...)
  
  if (length(graficos) > nrow * ncol) {
    stop("La cantidad de gráficos supera el tamaño de la grilla.")
  }
  
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = nrow, ncol = ncol)))
  
  for (i in seq_along(graficos)) {
    fila <- ceiling(i / ncol)
    columna <- ((i - 1) %% ncol) + 1
    
    print(
      graficos[[i]],
      vp = grid::viewport(layout.pos.row = fila, layout.pos.col = columna)
    )
  }
  
  grid::popViewport()
}

# === Tasa al contado de bonos corporativos
# de mercado de alta calidad (HQM) a 12 años==== 

# Base de datos con la serie importada a R

library(readr)

datos <- read_csv("Datos/HQMCB12YR.csv", 
                  col_names = TRUE, 
                  show_col_types = FALSE)

# Ver el tipo de objeto de la base de datos (tibble/data.frame)
print(class(datos))

# Ver primeras y últimas observaciones de la base de datos
print(head(datos)) # Primeras observaciones
print(tail(datos)) # Últimas observaciones

# Creación de la serie de tiempo de "Tasas al contado" ---

serie_bon <- datos
colnames(serie_bon)[2] <- "prices"


#Las volvemos ts y xts

btts = ts(serie_bon$prices, start = 1984, frequency = 12)
btxts = xts(serie_bon$prices, 
            order.by = serie_bon$observation_date) 

t = as.vector(t(serie_bon$prices))
ts = ts(t[1:508], start = c(1984), frequency = 12)


#Graficamos la serie

plot(btxts, main = "Tasa SPOT bonos corporativos
 de mercado de alta calidad (HQM) a 12 años ",
     sub = "1984-2026",
     ylab  = "%")

#Graficamos las FAC y las FACP

lags <- 24

x11()

par(mfrow = c(1, 2))
acf(btts, lag.max = lags, plot = TRUE, lwd = 2, xlab = '', main = 'ACF', ylim = c(-1, 1)) 
pacf(btts, lag.max = lags, plot = TRUE, lwd = 2, xlab = '', main = 'PACF', ylim = c(-1, 1))

#Prueba D-F para la serie normal

resultado_adf <- adf.test(btts)
print(resultado_adf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Transformación para volver estacionaria la serie #### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#  Aplicar diff() : 

d.btts= diff(btts) # Serie diferenciada

# Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su 
# valor en logaritmos.

x11()
par(mfrow=c(1,2))

plot.ts(btts, xlab="",ylab="", 
        main="Serie Normal",lty=1, lwd=2, col="lightblue")
plot.ts(d.btts, xlab="",ylab="", 
        main="Variación de los bonos",lty=1, lwd=2, col="orange")

#Miramos las FAC y las FACP de la serie que escogimos 

lags <- 24
x11()
par(mfrow=c(1,2))

acf(d.btts, lag.max = lags, plot=T, lwd=2, xlab='', main='ACF', ylim=c(-1,1)) 
pacf(d.btts, lag.max = lags, plot=T, lwd=2, xlab='', main='PACF', ylim=c(-1,1))

par(mfrow=c(1,1))
#Prueba D-F para la serie diferencia

resultado_adf <- adf.test(d.btts)
print(resultado_adf)

#Se concluye que el proceso es estacionario

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.Identificacion del Modelo-Criterios de informacion ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

AR.m <- 6 
MA.m <- 6 

#Esta linea de codigo sirve para mostrar opciones de procesos ARIMA

arma_seleccion_df = function(ts_object, AR.m, MA.m, d, bool_trend, metodo){
  
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(ts_object, order = c(p, d, q), include.mean = bool_trend, 
                    method = metodo)
      df[index,] = c(p, d, q, AIC(fitp), BIC(fitp))
      index = index + 1
    }
  }  
  return(df)
}

arma_min_AIC = function(df){
  df2 = df %>% 
    filter(AIC == min(AIC))
  return(df2)
}


arma_min_BIC = function(df){
  df2 = df %>% 
    filter(BIC == min(BIC))
  return(df2)
}


mod_d1_bond = arma_seleccion_df(btts, AR.m, MA.m, d = 1, TRUE, "ML")

min_aic = arma_min_AIC(mod_d1_bond); min_aic
min_bic= arma_min_BIC(mod_d1_bond); min_bic

view(mod_d1_bond)

# Elegimos el modelo, ARIMA de orden (0,1,1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####    ESTIMACION    ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

arima_0.1.1 = Arima(btts, order = c(0,1,1), include.mean = T, 
                    method = "ML")
# Ver estimaciones, errores estándar, z-stat y p-values de los coeficientes

lmtest::coeftest(arima_0.1.1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Verificacion de supuestos de supuestos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### No autocorrelación de los errores ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# De lags usamos un cuarto de la muestra 

lags.test = length(btts)/4;lags.test


# Grafica de las autocorrelaciones

#ARIMA 0,1,1

x11()
res_arima_0.1.1 = residuals(arima_0.1.1)
par(mfrow=c(1,2))

acf(res_arima_0.1.1, lag.max = lags, plot=T, lwd=2, xlab='', main='ACF', ylim=c(-1,1)) 
pacf(res_arima_0.1.1, lag.max = lags, plot=T, lwd=2, xlab='', main='PACF', ylim=c(-1,1))

#~~ LJUNG-BOX ~~#

#ARIMA 0,1,1

Box.test(res_arima_0.1.1, lag=lags.test, type = c("Ljung-Box")) 
Box.test(res_arima_0.1.1 , lag=10, type='Ljung-Box') 
Box.test(res_arima_0.1.1 , lag=20, type='Ljung-Box') 
Box.test(res_arima_0.1.1 , lag=30, type='Ljung-Box') 

#Concluimos que el modelo no tiene autocorrelacion

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Homocedasticidad de los residuales ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Ho = Homocedasticidad 
#Ha = heterocedasticidad

library(FinTS)

arch_tasas_arima_0.1.1 = ArchTest(res_arima_0.1.1, lags = 4)

# Imprimimos el resultado en la consola
print(arch_tasas_arima_0.1.1)

#Si queremos obtener un unico p-value para un número de lags
#en especifico se puede utilizar: 

#Hallamos los residuos 
residuos <- residuals(arima_0.1.1)

#Realizamos la prueba

ArchTest(residuos, lags = 44.5)

# Grafica de los residuos al cuadrado

x11()
par(mfrow=c(1,2))
acf(res_arima_0.1.1^2,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF residuales al cuadrado', ylim=c(-1,1)) 
pacf(res_arima_0.1.1^2,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF residuales al cuadrado', ylim=c(-1,1))
par(mfrow=c(1,1))

#Evidenciamos una fuerte heterocedasticidad

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Normalidad en los residuales ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#--> ARIMA(0,1,1)
x11()
qqPlot(res_arima_0.1.1, ylab = "ARIMA(0,1,1)")

#Vemos colas pesadas

# Prueba formal: Jarque-Bera Test

#Ho = Normalidad
#Ha = No hay normalidad

jarque.bera.test(res_arima_0.1.1) 

#Se confirma no normalidad 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Cambia con dummys para los datos atípicos? ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Obtener la longitud total de la serie original
longitud_diff <- length(d.btts)
dummy_atipicos <- rep(0, longitud_diff)

#Asignar un 1 en las posiciones 298 y 299
dummy_atipicos[c(297, 298)] <- 1

arima_0.1.1_corregido <- arima(d.btts, 
                               order = c(0, 0, 1), 
                               include.mean = FALSE, 
                               method = "ML", 
                               xreg = dummy_atipicos)
stargazer(arima_0.1.1_corregido,type="text",style = "aer")

print(arima_0.1.1_corregido)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Verificacion de supuestos de supuestos con serie with Dummy####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### No autocorrelación de los errores ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# De lags usamos un cuarto de la muestra 

lags.test = length(btts)/4;lags.test


# Grafica de las autocorrelaciones

#ARIMA 0,1,1

x11()
res_arima_0.1.1_corregido = residuals(arima_0.1.1_corregido)
par(mfrow=c(1,2))

acf(res_arima_0.1.1_corregido,lag.max=24,plot=T,lwd=1,xlab='',
    main='ACF residuales (0,1,1)', ylim=c(-1,1)) 

pacf(res_arima_0.1.1_corregido,lag.max=24,plot=T,lwd=1,xlab='',
     main='ACF al cuadrado residuales (0,1,1)', ylim=c(-1,1))
par(mfrow=c(1,1))

# Pruebas formales:

#~~ LJUNG-BOX ~~#

#ARIMA 0,1,1
Box.test(res_arima_0.1.1_corregido, lag=lags.test, type = c("Ljung-Box")) 
Box.test(res_arima_0.1.1_corregido , lag=10, type='Ljung-Box') 
Box.test(res_arima_0.1.1_corregido , lag=20, type='Ljung-Box') 
Box.test(res_arima_0.1.1_corregido , lag=30, type='Ljung-Box') 



# SE CUMPLE NO AUTOCORRELACIÓN !!!!!!!!!!!!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Homocedasticidad de los residuales ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Ho = Homocedasticidad 
#Ha = heterocedasticidad

arch_deuda_arima_0.1.1_corregido = arch.test(arima_0.1.1_corregido, output=TRUE)

#Se confirma heterocedasticidad 

#Si queremos obtener un unico p-value para un número de lags
#en especifico se puede utilizar: 

#Hallamos los residuos 
residuos_corregido <- residuals(arima_0.1.1_corregido)

#Realizamos la prueba

ArchTest(residuos_corregido, lags = 44.5)

#Rechazamos la H0, hay heterocedasticidad

# Grafica de los residuos al cuadrado

x11()
par(mfrow=c(1,2))
acf(res_arima_0.1.1^2,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF residuales al cuadrado', ylim=c(-1,1)) 
pacf(res_arima_0.1.1^2,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF residuales al cuadrado', ylim=c(-1,1))
par(mfrow=c(1,1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Normalidad en los residuales ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#--> ARIMA(0,1,1)
x11()
qqPlot(res_arima_0.1.1_corregido, ylab = "ARIMA(0,1,1)")

#Vemos colas pesadas

# Prueba formal: Jarque-Bera Test

#Ho = Normalidad
#Ha = No hay normalidad


jarque.bera.test(res_arima_0.1.1_corregido) 

#Concluimos que no hay normalidad

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Pronóstico con la serie with dummys ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# La diferenciación no es manual si no la hace el comando
# ARIMA.

# ARIMA(0,1,1) sobre la variable base dif

library(forecast)
library(lmtest)

# Creamos la variable dummy con la longitud correcta para la serie original
longitud_original <- length(btts)
dummy_fable <- rep(0, longitud_original)
dummy_fable[c(298, 299)] <- 1

# Estimamos el ARIMA(0,1,1) usando Arima pasándole los datos y la dummy
# Al usar la serie original btts con order=c(0,1,1)
modelo_clasico_OK <- Arima(btts, 
                           order = c(0, 1, 1), 
                           include.mean = FALSE, 
                           method = "ML", 
                           xreg = dummy_fable)

# Revisamos que todo esté ok
coeftest(modelo_clasico_OK)

# Creamos un vector de ceros para la dummy en el futuro (10 periodos adelante)
# Asumimos que en el futuro la economía se comporta normal y no se repite el shock
dummy_futura <- rep(0, 10)

# Forzamos a R a usar el 'forecast' clásico con 'forecast::'
pronostico_final <- forecast::forecast(modelo_clasico_OK, h = 10, xreg = dummy_futura)


print(pronostico_final)

# Graficamos el resultado
plot(pronostico_final, 
     main = "Pronóstico sobre Serie Original", 
     xlab = "Tiempo", 
     ylab = "%", 
     col = "black", 
     fcol = "blue")

# Calculamos el ajuste para tus datos
# Usamos tu serie original 'btts' porque tu modelo 'modelo_clasico_OK' ya maneja la diferencia por dentro
fit_tus_datos <- btts - residuals(modelo_clasico_OK)

# Graficamos la serie real (Línea Negra)
plot.ts(btts, type = "l",
        main = "Serie Observada VS Ajuste del Modelo ARIMA(0,1,1)",
        xlab = "Años",
        ylab = "%",
        lwd = 1)

# Superponemos la estimación del modelo (Línea Roja)
points(fit_tus_datos, col = "firebrick1", lwd = 1.5, type = "l")

# Leyenda
legend("topleft", 
       legend = c("Observada (Real)", "Ajustada (Modelo)"), 
       col = c("black", "firebrick1"), 
       lty = 1, 
       lwd = 2,
       bty = "n")

#Analizamos la resta/diferencia de la serie real vs estimada con ARIMA (0,1,1)

# Calculamos la resta (Real - Estimada)
resta_residuos <- btts - fit_tus_datos

# Graficamos la resta en el tiempo
plot.ts(resta_residuos,
        main = "Diferencia entre Serie Real y Estimada (Residuos)",
        xlab = "Años",
        ylab = "Diferencia (Errores)",
        col = "black",
        lwd = 1)

# Añadimos una línea de referencia en el CERO
# Es ruido blanco, los datos  oscilan simétricamente alrededor de esta línea
abline(h = 0, col = "firebrick1", lty = 2, lwd = 2)


#Fin del código
