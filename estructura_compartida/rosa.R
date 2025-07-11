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
    title = "Prestaciones por modalidad",
    subtitle = "Por comunidad autónoma"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    `Administración directa por el equipo sanitario ` = "Administración directa",
    `Autoadministración sin distinción por vía de administración` = "Autoadministración sin distinción",
    `Autoadministración por vía oral` = "Autoadministración vía oral",
    `Autoadministración por vía intravenosa` = "Autoadministración vía intravenosa",
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
    title = "Lugar de realización de la prestación",
    subtitle = "Por comunidad autónoma"
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
    title = "Ámbito público o privado",
    subtitle = "Por comunidad autónoma"
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
## 5.5. Denegaciones de MR respecto a denegaciones totales (excel 1)
## 5.6. Denegaciones de MC respecto a denegaciones totales (excel 1)
## 5.7. Denegaciones de CGyE respecto a denegaciones totales (excel 1)
## 5.8. Tasa de autorización de la CGyE
tab5_8 <- df %>%
  group_by(CCAA) %>%
  summarise(
    Informe_favorable_CGyE = sum(InformeCGyE == 1, na.rm = TRUE),
    Total_filas = n(),
    Tasa_informe_favorable = Informe_favorable_CGyE / Total_filas * 100
  ) %>%
  ungroup()

# Calcular fila total nacional
nacional <- tab5_8 %>%
  summarise(
    CCAA = "Total",
    Informe_favorable_CGyE = sum(Informe_favorable_CGyE, na.rm = TRUE),
    Total_filas = sum(Total_filas, na.rm = TRUE),
    Tasa_informe_favorable = Informe_favorable_CGyE / Total_filas * 100
  )

# Unir tabla principal con fila total y mostrar
final_tab5_8 <- bind_rows(tab5_8, nacional) %>%
  gt() %>%
  tab_header(
    title = "Tasa de informes favorables de la Comisión de Garantía y Evaluación (CGyE)",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    Informe_favorable_CGyE = "Número de informes favorables",
    Total_filas = "Num. solicitudes",
    Tasa_informe_favorable = "Tasa de informes favorables (%)"
  ) %>%
  fmt_number(
    columns = Tasa_informe_favorable,
    decimals = 2
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = CCAA == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos oficiales sobre prestaciones de PAM."
  )

gtsave(final_tab5_8, "tablas/tab5_8.html")

#5.1. RECLAMACIONES DE LA PAM
## Reclamaciones por instancia ante la que se presenta: CGyE vs jurisdicción contencioso-administrativa (borrador).

## 5.1.2. Reclamaciones por resolución de la CGyE: favorable o desfavorable por comunidad autónoma - excel 8

df <- read.csv2("Reclamaciones_CGyE.csv", stringsAsFactors = FALSE)

# Leer estructura
head(df)
str(df)
# Cambiamos el nombre de la primera columna
names(df)[1] <- "Reclamaciones"

# Transformar formato largo
df_largo <- df %>%
  pivot_longer(
    cols = -Reclamaciones,
    names_to = "CCAA",
    values_to = "Numero"
  )
# Comprobar
head(df_largo)
nrow(df_largo)

# Reformatear para poner las CCAA como filas y el ámbito como columnas y quitar los vacíos 
df_reformateado <- df_largo %>%
  filter(Reclamaciones != "" & !is.na(Reclamaciones)) %>%  # quita valores vacíos o NA
  pivot_wider(
    names_from = Reclamaciones,
    values_from = Numero
  )

# Guardar el archivo transformado
library(readr)
write_csv(df_reformateado, "Reclamaciones_CGyE_CCAA.csv")

# Tabla
tabla5_1_2 <- df_reformateado %>%
  gt() %>%
  tab_header(
    title = "Reclamaciones por resolución de la CGyE",
    subtitle = "favorable o desfavorable por comunidad autónoma"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    `Estimatorias de la pretensión del paciente` = "Estimatorias",
    `Desestimatorias de la pretensión del paciente` = "Desestimatorias",
    `Reclamaciones que permanecen a la espera de una resolución` = "A la espera de resolución",

  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos oficiales sobre prestaciones de PAM."
  )

# Guardar tabla como archivo HTML
gtsave(tabla5_1_2, "tablas/tabla5_1_2.html")



#6. FALLECIMIENTOS DURANTE LA TRAMITACIÓN DE LA PAM
## 6.1. Fallecimientos por comunidad autónoma y sexo
tab6_1 <- df %>%
  filter(Fallecimiento.durante.tramitación == 1) %>%
  mutate(
    CCAA = trimws(as.character(CCAA)),
    SEXO = toupper(trimws(as.character(SEXO))),
    CCAA = ifelse(is.na(CCAA) | CCAA == "" | CCAA == "PERDIDOS", "Desconocido", CCAA),
    SEXO = ifelse(SEXO %in% c("H", "M"), SEXO, NA)
  ) %>%
  filter(!is.na(SEXO)) %>%
  group_by(CCAA, SEXO) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = SEXO, values_from = n, values_fill = 0) %>%
  mutate(
    H = if ("H" %in% names(.)) H else 0,
    M = if ("M" %in% names(.)) M else 0,
    Total = H + M
  )

# Calcular totales nacionales por sexo y total general
nacional <- tab6_1 %>%
  summarise(
    CCAA = "Total",
    H = sum(H, na.rm = TRUE),
    M = sum(M, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE)
  )

# Unir tabla principal con fila de totales nacionales y mostrar
final_tab6_1 <- bind_rows(tab6_1, nacional) %>%
  gt() %>%
  tab_header(
    title = "Fallecimientos durante la tramitación de la PAM por CCAA y sexo",
    subtitle = "Número de fallecimientos"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    H = "Hombres",
    M = "Mujeres",
    Total = "Total"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = CCAA == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(final_tab6_1, "tablas/tab6_1.html")

## 6.2. Fallecimientos por tramo de edad _ datos brutos
tab6_2 <- df %>%
  filter(Fallecimiento.durante.tramitación == 1) %>%
  mutate(
    CCAA = trimws(as.character(CCAA)),
    TRAMO_EDAD = trimws(as.character(TRAMO_EDAD)),
    CCAA = ifelse(is.na(CCAA) | CCAA == "" | CCAA == "PERDIDOS", "Desconocido", CCAA)
  ) %>%
  group_by(TRAMO_EDAD) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    TRAMO_EDAD = case_when(
      TRAMO_EDAD == "1-Tramo de edad (<30)" ~ "Menores de 30 años",
      TRAMO_EDAD == "2-Tramo de edad (30-39)" ~ "Entre 30 y 39 años",
      TRAMO_EDAD == "3-Tramo de edad (40-49)" ~ "Entre 40 y 49 años",
      TRAMO_EDAD == "4-Tramo de edad (50-59)" ~ "Entre 50 y 59 años",
      TRAMO_EDAD == "4-Tramo de edad (60-69)" ~ "Entre 60 y 69 años",
      TRAMO_EDAD == "5-Tramo de edad (70-79)" ~ "Entre 70 y 79 años",
      TRAMO_EDAD == "6-Tramo de edad (> 80)" ~ "Mayores de 80 años",
      TRUE ~ TRAMO_EDAD
    )
  )

# Calcular fila total
nacional_tramo <- tab6_2 %>%
  summarise(
    TRAMO_EDAD = "Total",
    n = sum(n, na.rm = TRUE)
  )

# Unir tabla principal con fila total y mostrar
final_tab6_2 <- bind_rows(tab6_2, nacional_tramo) %>%
  gt() %>%
  tab_header(
    title = "Fallecimientos durante la tramitación por tramo de edad",
    subtitle = "Total nacional"
  ) %>%
  cols_label(
    TRAMO_EDAD = "Tramo de edad",
    n = "Número de fallecimientos"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = TRAMO_EDAD == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(final_tab6_2, "tablas/tab6_2.html")



## 6.3. Fallecimientos por enfermedad de base y año 2021-24
tab6_3 <- df %>%
  filter(Fallecimiento.enfermedad.base == 1) %>%
  group_by(CCAA, Fallecimiento.enfermedad.base) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Fallecimiento.enfermedad.base, values_from = n, values_fill = 0)

# Calcular totales nacionales por enfermedad de base y total general
nacional <- tab6_3 %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(CCAA = "Total")

# Unir tabla principal con fila de totales nacionales y mostrar
final_tab6_3 <- bind_rows(tab6_3, nacional) %>%
  gt() %>%
  tab_header(
    title = "Fallecimientos por enfermedad de base",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma"
    # Puedes añadir etiquetas personalizadas para cada enfermedad si lo deseas
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = CCAA == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  )

gtsave(final_tab6_3, "tablas/tab6_3.html")

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
    title = "Fallecimientos por tipo de enfermedad",
    subtitle = "Por comunidad autónoma"
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos oficiales sobre prestaciones de PAM."
  )

# Guardar tabla como archivo HTML
gtsave(tabla6_4, "tablas/tabla6_4.html")

## Media y mediana de días desde el inicio de la tramitación hasta el fallecimiento por comunidad autónoma
## Tasa de fallecimientos según fase del trámite respecto a solicitudes totales

