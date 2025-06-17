#Script de pruebas
library(tidyverse)
library(gt)

# Randomizar datos
#df_socio<-sample_n(df[,c(1, 4:7)], 200)
#df_fecha<-sample_n(df[, -c(1, 4:7)], 200)
#df<-cbind(df_socio, df_fecha)

# Cargar los datos
df<-read.csv("data_sim.csv")
glimpse(df)

# Formato de fecha
temp <- as.Date(df$FECHA_CIERRE, format="%d/%m/%y")
format(temp, format='%d-%m-%Y')


# Generar tablas
table(df$CCAA)

# Crear una tabla con el número de solicitudes por CCAA
tab1<-df %>%
  group_by(CCAA) %>%
  summarise(n = n()) %>%
#  arrange(desc(n)) %>% #Ordenar de mayor a menor
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de solicitudes de PAM",
    subtitle = glue::glue("Por Comunidad Autónoma")
  ) %>%
  tab_spanner( # Añadir spanner para las columnas
    label = "Resultados",
    columns = c(CCAA, n)
  ) %>%
  cols_label( # Etiquetas de las columnas
    CCAA = "Com. Aut.",
    n = "Número de solicitudes"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) %>%
  tab_source_note( # Añadir una nota adicional
    source_note = md("Nota: Se muestran las solicitudes totales por comunidad autónoma.")
  ) %>%
  tab_footnote( # Añadir una nota al pie de la tabla, asociada a una celda específica
    footnote = "Anotación de pie de página",
    locations = cells_body(columns = CCAA, rows = 3:4)
  )

# Exportar la tabla a HTML
tab1 %>%
  gtsave("tablas/tab1_ej.html")

