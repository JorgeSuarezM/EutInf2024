# Preparación de datos

## Librerías
library(tidyverse)
library(gt)

# Cargar los datos
df<-read.csv("data_sim.csv")
glimpse(df)

# Formato de fecha
temp <- as.Date(df$FECHA_CIERRE, format="%d/%m/%y")
format(temp, format='%d-%m-%Y')
