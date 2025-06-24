#4.2. ASPECTOS CLÍNICOS Y DE ACTUACIÓN DEL PERSONAL SANITARIO
## Prestaciones por modalidad y año 2021-24
# Cargar el archivo y modificar el delimitador
file_path <- "Modalidad.csv"
df <- read.csv(file_path, sep = ";")
write.csv(df, file_path, row.names = FALSE)
df <- read.csv(file_path, sep = ",")


df_filtered <- df %>%
    select(
        CCAA, 
        Administración.directa.por.el.equipo.sanitario, 
        Autoadministración.sin.distinción.por.vía.de.administración, 
        Autoadministración.por.vía.oral, 
        Autoadministración.por.vía.intravenosa
    ) %>%
    group_by(CCAA) %>%
    summarise(
        Directa_equipo_sanitario = sum(Administración.directa.por.el.equipo.sanitario, na.rm = TRUE),
        Autoadministración_total = sum(Autoadministración.sin.distinción.por.vía.de.administración, na.rm = TRUE),
        Autoadministración_oral = sum(Autoadministración.por.vía.oral, na.rm = TRUE),
        Autoadministración_intravenosa = sum(Autoadministración.por.vía.intravenosa, na.rm = TRUE)

    ) %>%

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Modalidad de la prestación",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  
    cols_label( # Renombrar las columnas
        CCAA = "Comunidad Autónoma",
        Directa_equipo_sanitario = "Administración directa por el equipo sanitario",
        Autoadministración_total = "Autoadministración total",
        Autoadministración_oral = "Autoadministración por vía oral",
        Autoadministración_intravenosa = "Autoadministración por vía intravenosa"
    ) %>%

gtsave(tab4_2_1, "tablas/tab4_2_1.html")



## Lugar de realización de la prestación por comunidad autónoma
## Ámbito público o privado por comunidad autónoma
## Tasa de prestaciones en ámbito público/privado
## Tasa de prestaciones en domicilio



#5. DENEGACIONES DE LA PAM
## Comunidad autónoma y sexo
## Enfermedad de base y año 2021-24
## Enfermedad de base y comunidad autónoma
## Tramo de edad
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
## Reclamaciones por resolución de la CGyE: favorable o desfavorable por comunidad autónoma



#6. FALLECIMIENTOS DURANTE LA TRAMITACIÓN DE LA PAM
## Fallecimientos por comunidad autónoma y sexo
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

## Fallecimientos por tramo de edad

# Falta la variable tramo de edad



## Fallecimientos por enfermedad de base y año 2021-24

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

## Fallecimientos por tipo de enfermedad

# No está la variable 

## Media y mediana de días desde el inicio de la tramitación hasta el fallecimiento por comunidad autónoma
## Tasa de fallecimientos según fase del trámite respecto a solicitudes totales

