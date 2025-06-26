#4.2. ASPECTOS CLÍNICOS Y DE ACTUACIÓN DEL PERSONAL SANITARIO
## 4.2.1 Prestaciones por modalidad y año 2021-24
# Cargar el archivo y modificar el delimitador
df <- read.csv2("Modalidad.csv", stringsAsFactors = FALSE)

# Leer estructura
head(df)
str(df)

# Transformar formato largo
df_largo <- df %>%
  pivot_longer(
    cols = -Modalidad,
    names_to = "CCAA",
    values_to = "Numero"
  )
# Comprobar
head(df_largo)
nrow(df_largo)

# Reformatear para poner las CCAA como filas y los modalidad como columnas quitando los vacíos
df_reformateado <- df_largo %>%
  filter(Modalidad != "" & !is.na(Modalidad)) %>%  # quita valores vacíos o NA
  pivot_wider(
    names_from = Modalidad,
    values_from = Numero
  )
# Guardar el archivo transformado
library(readr)
write_csv(df_reformateado, "Modalidad_por_CCAA.csv")

# Tabla
tabla4_2_1 <- df_reformateado %>%
  gt() %>%
  tab_header(
    title = "Número de prestaciones de la ayuda para morir",
    subtitle = "Por comunidad autónoma y modalidad"
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos oficiales sobre prestaciones de PAM."
  )

# Guardar tabla como archivo HTML
gtsave(tabla4_2_1, "tablas/tabla4_2_1.html")


## 4.2.2 Lugar de realización de la prestación por comunidad autónoma

# Cargar el archivo y modificar el delimitador
df <- read.csv2("Lugar_prestacion.csv", stringsAsFactors = FALSE)

# Leer estructura
head(df)
str(df)

# Cambiamos el nombre de la primera columna
names(df)[1] <- "Lugar"

# Transformar formato largo
df_largo <- df %>%
  pivot_longer(
    cols = -Lugar,
    names_to = "CCAA",
    values_to = "Numero"
  )
# Comprobar
head(df_largo)
nrow(df_largo)

# 5. Reformatear para poner las CCAA como filas y los lugares como columnas
df_reformateado <- df_largo %>%
  pivot_wider(
    names_from = Lugar,
    values_from = Numero
  )

# Guardar el archivo transformado
library(readr)
write_csv(df_reformateado, "Lugar_prestacion_por_CCAA.csv")

# Tabla
tabla4_2_2 <- df_reformateado %>%
  gt() %>%
  tab_header(
    title = "Número de prestaciones de la ayuda para morir",
    subtitle = "Por comunidad autónoma y lugar de prestación"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    `Nº de prestaciones de ayuda para morir realizadas en el hospital` = "Hospital",
    `Nº de prestaciones de ayuda para morir realizadas en centros no hospitalarios` = "Centros no hospitalarios",
    `Nº de prestaciones de ayuda para morir realizadas en el domicilio del paciente` = "Domicilio particular",
    `Nº de prestaciones de ayuda para morir realizadas en instituciones sociosanitarias` = "Centro sociosanitario"
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos oficiales sobre prestaciones de PAM."
  )

# Guardar tabla como archivo HTML
gtsave(tabla4_2_2, "tablas/tabla4_2_2.html")

## 4.2.3 Ámbito público o privado por comunidad autónoma

# Cargar el archivo y modificar el delimitador
df <- read.csv2("Ambito.csv", stringsAsFactors = FALSE)

# Leer estructura
head(df)
str(df)

# Transformar formato largo
df_largo <- df %>%
  pivot_longer(
    cols = -Ambito,
    names_to = "CCAA",
    values_to = "Numero"
  )
# Comprobar
head(df_largo)
nrow(df_largo)

# 5. Reformatear para poner las CCAA como filas y el ámbito como columnas y quitar los vacíos 
df_reformateado <- df_largo %>%
  filter(Ambito != "" & !is.na(Ambito)) %>%  # quita valores vacíos o NA
  pivot_wider(
    names_from = Ambito,
    values_from = Numero
  )

# Guardar el archivo transformado
library(readr)
write_csv(df_reformateado, "Ambito_por_CCAA.csv")

# Tabla
tabla4_2_3 <- df_reformateado %>%
  gt() %>%
  tab_header(
    title = "Número de prestaciones de la ayuda para morir",
    subtitle = "Por comunidad autónoma y ámbito"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    `Nº de prestaciones de ayuda para morir realizadas desde servicios públicos` = "Servicios públicos",
    `Nº de prestaciones de ayuda para morir realizadas desde servicios privados` = "Servicios privados",
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos oficiales sobre prestaciones de PAM."
  )

# Guardar tabla como archivo HTML
gtsave(tabla4_2_3, "tablas/tabla4_2_3.html")

## 4.2.4. Tasa de prestaciones en ámbito público/privado

# 1. Leer el CSV
df <- read_csv("Ambito_por_CCAA.csv")

# 2. Calcular tasa
df <- df %>%
  rename(
    Publico = `Nº de prestaciones de ayuda para morir realizadas desde servicios públicos`,
    Privado = `Nº de prestaciones de ayuda para morir realizadas desde servicios privados`
  ) %>%
  mutate(
    Total = Publico + Privado,
    Tasa_publico_sobre_total = Publico / Total
  )
# Redondeo de la tasa a dos decimales
df <- df %>%
  mutate(Tasa_publico_sobre_total = round(Tasa_publico_sobre_total, 2))
# Resultado
df %>%
  select(CCAA, Publico, Privado, Total, Tasa_publico_sobre_total)

# Tabla
  tabla4_2_4 <- df %>%
  gt() %>%
  tab_header(
    title = "Tasa de prestaciones realizadas en servicios públicos",
    subtitle = "Respecto al total, por comunidad autónoma"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    Tasa_publico_sobre_total = "Tasa público/total"
  ) %>%
  fmt_percent(
    columns = Tasa_publico_sobre_total,
    decimals = 1
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos oficiales sobre prestaciones de la ayuda para morir."
  )

gtsave(tabla4_2_4, "tablas/tabla4_2_4.html")

## 4.2.5. Tasa de prestaciones en domicilio
# 1. Leer el CSV
df <- read_csv("Lugar_prestacion_por_CCAA.csv")

# 2. Calcular tasa
df <- df %>%
  rename(
    Domicilio = `Nº de prestaciones de ayuda para morir realizadas en el domicilio del paciente`,
    Hospital = `Nº de prestaciones de ayuda para morir realizadas en el hospital`,
    Centros = `Nº de prestaciones de ayuda para morir realizadas en centros no hospitalarios`,
    Instituciones = `Nº de prestaciones de ayuda para morir realizadas en instituciones sociosanitarias`
  ) %>%
  mutate(
    Total = Domicilio + Hospital + Centros + Instituciones,
    Tasa_domicilio_sobre_total = Domicilio / Total
  )
# Redondeo de la tasa a dos decimales
df <- df %>%
  mutate(Tasa_domicilio_sobre_total = round(Tasa_domicilio_sobre_total, 2))
# Resultado
df %>%
  select(CCAA, Domicilio, Tasa_domicilio_sobre_total)

# Tabla
  tabla4_2_5 <- df %>%
  gt() %>%
  tab_header(
    title = "Tasa de prestaciones realizadas en domicilio",
    subtitle = "Respecto al total, por comunidad autónoma"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    Tasa_domicilio_sobre_total = "Tasa domicilio/total"
  ) %>%
  fmt_percent(
    columns = Tasa_domicilio_sobre_total,
    decimals = 1
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos oficiales sobre prestaciones de la ayuda para morir."
  )

gtsave(tabla4_2_5, "tablas/tabla4_2_5.html")


#5. DENEGACIONES DE LA PAM
## 5.1. Comunidad autónoma y sexo
## 5.2. Enfermedad de base y año 2021-24
## 5.3. Enfermedad de base y comunidad autónoma
## 5.4. Tramo de edad
## Denegaciones de MR respecto a denegaciones totales (excel 1)
## Denegaciones de MC respecto a denegaciones totales (excel 1)
## Denegaciones de CGyE respecto a denegaciones totales (excel 1)
## Tasa de autorización de la CGyE
tab5_8 <- df %>%
    group_by(CCAA) %>%
    summarise(
        Informe_favorable_CGyE = sum(Informe.favorable.Comisión.Garantía.y.Evaluación..CGyE. == 1, na.rm = TRUE),
        Total_filas = n(),
        Tasa_informe_favorable = Informe_favorable_CGyE / Total_filas * 100
    ) %>%

    gt() %>%
    tab_header( # Añadir título y subtítulo
        title = "Tasa de informes favorables de la Comisión de Garantía y Evaluación (CGyE)",
        subtitle = "Por Comunidad Autónoma"
    ) %>%
    cols_label( # Etiquetas de las columnas
        CCAA = "Comunidad Autónoma",
        Informe_favorable_CGyE = "Número de informes favorables",
        Total_filas = "Num. solicitudes",
        Tasa_informe_favorable = "Tasa de informes favorables (%)"
    ) %>%
    fmt_number(
        columns = Tasa_informe_favorable,
        decimals = 2
    )

gtsave(tab5_8, "tablas/tab5_8.html")

#5.1. RECLAMACIONES DE LA PAM
## Reclamaciones por instancia ante la que se presenta: CGyE vs jurisdicción contencioso-administrativa (borrador).

## 5.1.2. Reclamaciones por resolución de la CGyE: favorable o desfavorable por comunidad autónoma - excel 8





#6. FALLECIMIENTOS DURANTE LA TRAMITACIÓN DE LA PAM
## 6.1. Fallecimientos por comunidad autónoma y sexo
tab6_1<-df %>%
  filter(Fallecimiento.durante.tramitación == 1) %>% 
  group_by(CCAA, SEXO) %>%
  summarise(n = n()) %>%

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de fallecimientos durante la tramitación de la PAM",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label( # Etiquetas de las columnas
    SEXO = "CCAA y Sexo",
    n = "Número de fallecimientos"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab6_1, "tablas/tab6_1.html")

## 6.2. Fallecimientos por tramo de edad _ datos brutos






## 6.3. Fallecimientos por enfermedad de base y año 2021-24

tab6_3<-df %>%
  filter(Fallecimiento.enfermedad.base == 1) %>% 
  group_by(CCAA, SEXO) %>%
  summarise(n = n()) %>%

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de fallecimientos por enfermedad de base",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label( # Etiquetas de las columnas
    SEXO = "CCAA y Sexo",
    n = "Número de fallecimientos"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab6_3, "tablas/tab6_3.html")

## 6.4. Fallecimientos por tipo de enfermedad - excel 12

# Cargar el archivo y modificar el delimitador
df <- read.csv2("fallecidos_antes_prestacion.csv", stringsAsFactors = FALSE)

# Leer estructura
head(df)
str(df)

# Cambiamos el nombre de la primera columna 
names(df)[1] <- "Causa"

# Transformar formato largo
df_largo <- df %>%
  pivot_longer(
    cols = -Causa,
    names_to = "CCAA",
    values_to = "Numero"
  )
# Comprobar
head(df_largo)
nrow(df_largo)

# Reformatear para poner las CCAA como filas y la causa como columnas quitando los vacíos
df_reformateado <- df_largo %>%
  filter(Causa != "" & !is.na(Causa)) %>%  # quita valores vacíos o NA
  pivot_wider(
    names_from = Causa,
    values_from = Numero
  )

# Guardar el archivo transformado
library(readr)
write_csv(df_reformateado, "Causa_fallecimiento_por_CCAA.csv")

# Tabla
tabla6_4 <- df_reformateado %>%
  gt() %>%
  tab_header(
    title = "Número de prestaciones de la ayuda para morir",
    subtitle = "Por comunidad autónoma y causa de fallecimiento"
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos oficiales sobre prestaciones de PAM."
  )

# Guardar tabla como archivo HTML
gtsave(tabla6_4, "tablas/tabla6_4.html")

## Media y mediana de días desde el inicio de la tramitación hasta el fallecimiento por comunidad autónoma
## Tasa de fallecimientos según fase del trámite respecto a solicitudes totales

