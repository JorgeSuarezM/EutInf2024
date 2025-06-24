##ESTRUCTURA DEL INFORME DE ESTADÍSTICAS DE LA PAM

#2. ESTADÍSTICAS GENERALES DE LA PAM
## Nº solicitudes por C.A 
tab2_1<-df %>%
    group_by(CCAA) %>%
    summarise(n = n()) %>%
    gt() %>%
    tab_header(title = "Nº de solicitudes de PAM por CCAA") %>%
    tab_spanner(label="Resultados", columns = c(CCAA, n)) %>%
    cols_label(CCAA = "Comunidad Autónoma", n = "Número de solicitudes") %>%
    tab_source_note(source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM.")
    
    gtsave(tab2_1, "tablas/tab2_1.html")

## Nº denegaciones por C.A No se pueden hacer

## Nº revocaciones por C.A Hecha en apartado de María B. Tabla 7_1

## Nº aplazamientos por C.A Hecha en apartado de María B. Tabla 8_4_1


## Nº fallecidos durante la tramitación por C.A
tab2_2<-df %>%
  filter(Fallecimiento.durante.tramitación == 1) %>% 
  group_by(CCAA) %>%
  summarise(n = n()) %>%

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de fallecidos durante la tramitación de la PAM",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label( # Etiquetas de las columnas
    CCAA = "Com. Aut.",
    n = "Número de fallecidos durante la tramitación de la PAM"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab2_2, "tablas/tab2_2.html")
## Tasas de mortalidad por PAM (entre total fallecidos por C.A. y total nacional)
tab2_3 <- df %>%
  group_by(CCAA) %>%
  summarise(
    total_solicitudes = n(),
    total_revocaciones = sum(Revocación == 1, na.rm = TRUE)
    ) %>%
  mutate(
    tasa_revocaciones = round((total_revocaciones / total_solicitudes) * 100, 2)
  ) %>%

  gt() %>%
  tab_header(
    title = "Tasas por Comunidad Autónoma",
    subtitle = "Revocaciones sobre solicitudes"
  ) %>%
  cols_label(
    CCAA = "Com. Aut.",
    total_solicitudes = "Solicitudes",
    total_revocaciones = "Revocaciones",
    tasa_revocaciones = "Tasa revocaciones (%)",
    ) %>%
  tab_source_note(
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) %>%

gtsave("tablas/tab7_6.html")
## Tasa de mortalidad nacional por PAM por total muertes
tab2_4 <- df %>%
  summarise(
    total_fallecidos = sum(FECHA_PRESTACION != "", na.rm = TRUE),
    total_muertes = 433.547 # Aproximación de muertes en España en 2024
  ) %>%
  mutate(tasa_mortalidad = round((total_fallecidos / total_muertes) * 1000, 2)) %>%

  gt() %>%
  tab_header(
    title = "Tasa de mortalidad nacional por PAM",
    subtitle = "Por cada 100.000 habitantes"
  ) %>%
  cols_label(
    total_fallecidos = "Total fallecidos durante la tramitación",
    total_muertes = "Total muertes en España (2024)",
    tasa_mortalidad = "Tasa de mortalidad (por 100.000 hab.)"
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  )
gtsave(tab2_4, "tablas/tab2_4.html")
## Relación entre las resoluciones alcanzadas en 2024 y las solicitudes que las originan.
## Esquema (figura, diagrama de flujos) general del procedimiento: entradas (solicitudes) y resultados de cada filtro (MR, MC, CGyE), con los fallecimientos, denegaciones, revocaciones y prestaciones definitivas.



#3. SOLICITUDES DE LA PAM
## Solicitudes por comunidad autónoma y sexo, con columna de total para ambas variables
tab3_1<-df %>%
  group_by(CCAA, SEXO) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(TOTAL = sum(n)) %>%

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Solicitudes de PAM por CCAA y sexo",
    subtitle = "Número de solicitudes"
  ) 
  gtsave(tab3_1, "tablas/tab_3_1.html")
## Solicitudes por modalidad de inicio (5.1/5.2): primera solicitud o instrucciones previas, designando o no un interlocutor, entre 2021 y 2024,

## Solicitudes por modalidad de inicio (5.1/5.2): primera solicitud o instrucciones previas, designando o no un interlocutor, por C.A.
## Solicitudes por enfermedad de base entre 2021 y 2024
df_sol_enfermedad_años <- data.frame(
Año = c(2021, 2022, 2023, 2024)
)

tab3_2<-df_sol_enfermedad_años %>%
    summarise(n = n()) %>%

gt() %>%
tab_header( # Añadir título y subtítulo
title = "Número de prestaciones por enfermedad de base",
subtitle = "2021-2024"
) %>%
cols_label( # Etiquetas de las columnas
n = "Número de solicitudes",
PATOLOGIA = "Enfermedad de base",
) %>%
tab_source_note( # Añadir nota de la fuente
source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
)

gtsave(tab3_2, "tablas/tab3_2.html")

## Solicitudes por tramo de edad y enfermedad de base
tab3_3<-df %>%
  group_by(TRAMO_EDAD, PATOLOGIA) %>%
  summarise(n = n()) %>%
  ungroup() %>%

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Solicitudes de PAM por tramo de edad y enfermedad de base",
    subtitle = "Número de solicitudes"
  ) %>%
  cols_label( # Etiquetas de las columnas
    TRAMO_EDAD = "Tramo de Edad",
    PATOLOGIA = "Enfermedad de Base",
    n = "Número de solicitudes"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."  
  )
  gtsave(tab3_3, "tablas/tab3_3.html")
## Solicitudes por informe favorable/desfavorable MR e informe favorable/desfavorable MC (filas 3 a 8 de excel 1)
## Nº de solicitudes en las que el médico/a responsable ha adelantado la tramitación de la segunda solicitud, por considerar que la pérdida de la capacidad de la persona solicitante para otorgar el consentimiento informado es inminente (plazo menor de 15 días
## Tasa de solicitudes de PAM nacionales y por comunidad autónoma por millón de habitantes



#4. PRESTACIONES DE LA PAM
#4.1. CARACTERÍSTICAS DE LA PERSONA SOLICITANTE
## Prestaciones por C.A y sexo
tab4_1_1<-df %>%
  filter(FECHA_PRESTACION != "") %>%
  group_by(CCAA, SEXO) %>%
  summarise(n = n()) %>%

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Prestaciones por CCAA y sexo",

  ) %>%
  cols_label( # Etiquetas de las columnas
    CCAA = "Com. Aut.",
    SEXO = "Sexo",
    n = "Número de prestaciones"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  )

gtsave(tab4_1_1, "tablas/tab4_1_1.html")
## Prestaciones por tramo de edad
tab4_1_2<-df %>%
  filter(FECHA_PRESTACION != "") %>% 
  group_by(TRAMO_EDAD) %>%
  summarise(n = n()) %>%
  mutate(TRAMO_EDAD = ifelse(TRAMO_EDAD == "1-Tramo de edad (<30)", "Menores de 30 años",
                             ifelse(TRAMO_EDAD == "2-Tramo de edad (30-39)", "Entre 30 y 39 años",
                                ifelse(TRAMO_EDAD == "3-Tramo de edad (40-49)", "Entre 40 y 49 años",
                                    ifelse(TRAMO_EDAD == "4-Tramo de edad (50-59)", "Entre 50 y 59 años",
                                        ifelse(TRAMO_EDAD == "4-Tramo de edad (60-69)", "Entre 60 y 69 años",
                                            ifelse(TRAMO_EDAD == "5-Tramo de edad (70-79)", "Entre 70 y 79 años",
                                                ifelse(TRAMO_EDAD == "6-Tramo de edad (> 80)", "Mayores de 80 años", TRAMO_EDAD)))))))) %>% # Cambiar etiquetas de tramo de edad

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de prestaciones",
    subtitle = "Por Tramo de Edad"
  ) %>%
  cols_label( # Etiquetas de las columnas
    TRAMO_EDAD = "Tramo de Edad",
    n = "Número de prestaciones"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab4_1_2, "tablas/tab4_1_2.html")
## Prestaciones por enfermedad de base y año 2021-24
df_prest_enf <- data.frame(
Año = c(2021, 2022, 2023, 2024),
n_prestaciones = c(21, 22, 23, 24), 
Enfermedad_de_base = c("Enfermedad A", "Enfermedad B", "Enfermedad C", "Enfermedad D")
)

tab4_1_3<-df_prest_enf %>%

gt() %>%
tab_header( # Añadir título y subtítulo
title = "Número de prestaciones por enfermedad de base",
subtitle = "2021-2024"
) %>%
cols_label( # Etiquetas de las columnas
n_prestaciones = "Número de prestaciones",
Enfermedad_de_base = "Enfermedad de base",
) %>%
tab_source_note( # Añadir nota de la fuente
source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
)

gtsave(tab4_1_3, "tablas/tab4_1_3.html")
## Relación prestaciones / solicitudes por comunidades autónomas de forma absoluta y con porcentajes 
tab4_1_4 <- df %>%
  group_by(CCAA) %>%
  summarise(
    total_solicitudes = n(),
    total_prestaciones = sum(FECHA_PRESTACION != "", na.rm = TRUE)
  ) %>%
  mutate(
    tasa_prestaciones = round((total_prestaciones / total_solicitudes) * 100, 2)
  ) %>%

  gt() %>%
  tab_header(
    title = "Relación Solicitudes / Prestaciones por Comunidad Autónoma",
    subtitle = "Total de solicitudes y prestaciones"
  ) %>%
  cols_label(
    CCAA = "Com. Aut.",
    total_solicitudes = "Solicitudes",
    total_prestaciones = "Prestaciones",
    tasa_prestaciones = "Tasa de prestaciones (%)"
  ) %>%
  tab_source_note(
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  )
  gtsave(tab4_1_4, "tablas/tab4_1_4.html")
## Tasa de prestaciones (entre total fallecimientos por C.A. y total nacional)
