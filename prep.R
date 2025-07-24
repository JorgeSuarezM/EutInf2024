# Preparación de datos

## Librerías
library(tidyverse)
library(gt)
library(gtExtras)
library(dplyr)

# Cargar los datos
df<-read.csv("data.csv", fileEncoding="latin1", sep=";")
glimpse(df)

# Establecer formato de fecha
df<-df %>%
mutate(fecha_cierre = as.Date(fecha_cierre, format="%d/%m/%Y"),
fecha_prestacion = as.Date(fecha_prestacion, format="%d/%m/%Y"),
fecha_informe_cgye = as.Date(fecha_informe_cgye, format="%d/%m/%Y"),
fecha_informe_mc = as.Date(fecha_informe_mc, format="%d/%m/%Y"),
fecha_informe_mr = as.Date(fecha_informe_mr, format="%d/%m/%Y"),
fecha_fallecimiento_tramitacion = as.Date(fecha_fallecimiento_tramitacion, format="%d/%m/%Y"),
fecha_aplazamiento = as.Date(fecha_aplazamiento, format="%d/%m/%Y"),
fecha_primera_solicitud = as.Date(fecha_primera_solicitud, format="%d/%m/%Y"),
fecha_segunda_solicitud = as.Date(fecha_segunda_solicitud, format="%d/%m/%Y"))

# Renombrar columnas para facilitar el uso
# (!) Comentar con ## las líneas siguientes para evitar conflictos con las tablas ya hechas
df<-df %>%
rename(informe_mr = informe_fav_mr,
    informe_mc = informe_fav_mc,
    informe_cgye = informe_fav_cgye,
    fecha_fallecimiento = fecha_fallecimiento_tramitacion)

# Crear variables de denegación y muerte durante la tramitación
df<-df %>%
mutate(reclamacion=if_else((reclamacion_cgye == 'Si' | resolucion_fav_cgye != ''), 1, 0),
    denegado=if_else(
    is.na(fecha_prestacion) & (informe_mr == 'No' | informe_mc == 'No' | informe_cgye == 'No') & resolucion_fav_cgye != 'Si', 1, 0),
    fallecimiento_tramitacion=if_else(fallecimiento_tramitacion == 'Si', 1, 0),
    fallecimiento_tramitacion_momento=case_when(
        fallecimiento_tramitacion != 1 ~ "No",
        fecha_fallecimiento <= fecha_informe_cgye ~ "1Previo a Comisión",
        fecha_fallecimiento <= fecha_informe_mc ~ "2Previo a Informe Médico Consultor",
        fecha_fallecimiento <= fecha_informe_mr ~ "3Previo a Informe Médico Responsable"
    ),
    revocacion=if_else(revocacion == 'Si', 1, 0)
)