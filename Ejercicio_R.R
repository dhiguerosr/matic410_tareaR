dir <- "~/Documents/MATIC/MATIC410/R"
setwd(dir)
# Matriz de datos da3240, del Bar?metro de octubre de 2009
anchos <- c(-9, 2, 2, -25, 1, -2, 1, -147, 1, 2)
nvar <- c('ccaa', 'prov', 'siteco', 'sitpol', 'sexo', 'edad')
d3240 <- read.fwf('DA3240', widths=anchos, col.names=nvar)
d3240[1:20,]; head(d3240) # La instrucci?n head saca por pantalla las primeras filas de la hoja de datos
names(d3240)
#---------------------------------------------------
# CONVERSI?N DE VARIABLES NUM?RICAS EN FACTORES
#---------------------------------------------------
b <- d3240
# Conversion de las variables en factores con bucle for
# (convertimos todas las variables excepto 'edad', que es la sexta y ?ltima)
for (i in 1:5) b[ , i] <- as.factor(b[ , i])
levels(b$siteco)
#---------------------------------------------
# MEDIDAS DE POSICI?N, DISPERSI?N Y FORMA
#---------------------------------------------
ed <- b$edad
ed <- ed[ed < 99]
#--------------
# Tarea 1
#--------------

# Crear una nueva variable ed2 basada en ed
ed2 <- ed

# Calcular la media de la variable ed2
# La función mean requiere datos numéricos válidos
media_ed2 <- mean(ed2, na.rm = TRUE)

# Asignar el valor NA a las observaciones de la 31 a la 50
ed2[31:50] <- NA

media_ed2_con_na_rm <- mean(ed2, na.rm = TRUE)
media_ed2_sin_na_rm <- mean(ed2)

serror <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]  # Filtrar valores NA si es necesario
  }
  return(sd(x) / sqrt(length(x)))
}

serror_ed2_con_na_rm <- serror(ed2, na.rm = TRUE)
serror_ed2_con_na_rm <- serror(ed2)

install.packages("RcmdrMisc")

library(RcmdrMisc)

numSummary_result <- numSummary(ed2, statistics = c("mean", "sd", "n", "na"))
print(numSummary_result)

#-------------------------------------------------------------------
# IMPORTACI?N DE DATOS DESDE FICHEROS DE SPSS (Ficheros .sav)
#-------------------------------------------------------------------
library(foreign)
# Si queremos que los factores nos aparezcan con su literal
d <- read.spss('3240.sav', to.data.frame=T)
d[1:20, 1:10]

#--------------
# Tarea 2
#--------------
names(d)

freq_CCAA <- table(d$CCAA)          # Frecuencias de la variable "CCAA"
freq_P14 <- table(d$P14)            # Frecuencias de la variable "P14"
freq_RECUERDO <- table(d$RECUERDO)  # Frecuencias de la variable "RECUERDO"
freq_ESTUDIOS <- table(d$ESTUDIOS)  # Frecuencias de la variable "ESTUDIOS"
freq_P27 <- table(d$P27)            # Frecuencias de la variable "P27"

cruce_CCAA_RECUERDO <- table(d$CCAA, d$RECUERDO)  # Cruce entre CCAA y RECUERDO
cruce_RECUERDO_P27 <- table(d$RECUERDO, d$P27)    # Cruce entre RECUERDO y P27
cruce_ESTUDIOS_P27 <- table(d$ESTUDIOS, d$P27)    # Cruce entre ESTUDIOS y P27

porc_filas_CCAA_RECUERDO <- prop.table(cruce_CCAA_RECUERDO, margin = 1) * 100     # Porcentaje filas CCAA y RECUERDO
porc_columnas_CCAA_RECUERDO <- prop.table(cruce_CCAA_RECUERDO, margin = 2) * 100  # Porcentaje columnas CCAA y RECUERDO

porc_filas_RECUERDO_P27 <- prop.table(cruce_RECUERDO_P27, margin = 1) * 100       # Porcentaje filas RECUERDO y P27
porc_columnas_RECUERDO_P27 <- prop.table(cruce_RECUERDO_P27, margin = 1) * 100    # Porcentaje columnas RECUERDO y P27
 
porc_filas_ESTUDIOS_P27 <- prop.table(cruce_ESTUDIOS_P27, margin = 1) * 100       # Porcentaje filas ESTUDIOS y P27
porc_columnas_ESTUDIOS_P27 <- prop.table(cruce_ESTUDIOS_P27, margin = 1) * 100    # Porcentaje columnas ESTUDIOS y P27

print(porc_filas_CCAA_RECUERDO)
print(porc_columnas_CCAA_RECUERDO)
print(porc_filas_RECUERDO_P27)
print(porc_columnas_RECUERDO_P27)
print(porc_filas_ESTUDIOS_P27)
print(porc_columnas_ESTUDIOS_P27)
#--------------
# Tarea 3
#--------------

h <- b
h <- na.omit(h)
h$intedad <- cut(
  as.numeric(h$edad), # Asegurarse de que la columna edad es numérica
  breaks = c(0, 7, 17, 24, 34, 44, 54, 64, Inf), # Definir los límites de los intervalos
  labels = c("0-7", "8-17","18-24", "25-34", "35-44", "45-54", "55-64", "65+"), # Etiquetas para los intervalos
  right = FALSE # Intervalos abiertos por la derecha (18 <= x < 25, etc.)
)

table(h$intedad) # Mostrar la distribución de los intervalos

cruce_sitpol_intedad <- table(h$sitpol, h$intedad)  #Cruce entre sitpol e intedad
cruce_ccaa_intedad <- table(h$ccaa, h$intedad)      #Cruce entre ccaa e intedad
cruce_sexo_intedad <- table(h$sexo, h$intedad)      #Cruce entre sexo e intedad

porcentaje_filas_ccaa_intedad <- prop.table(cruce_ccaa_intedad, margin = 1) * 100
porcentaje_columnas_sexo_intedad <- prop.table(cruce_sexo_intedad, margin = 2) * 100

test_chi2 <- chisq.test(cruce_ccaa_intedad)
print(test_chi2)

cruce_ccaa_sitpol_por_sexo <- with(h, table(ccaa, sitpol, sexo))
print(cruce_ccaa_sitpol_por_sexo)
