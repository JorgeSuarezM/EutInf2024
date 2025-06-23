#7. REVOCACIONES DE LA PAM
## Revocaciones por comunidad autónoma (excel bruto)
tab7_1<-df %>%
  filter(Revocación == 1) %>% 
  group_by(CCAA) %>%
  summarise(n = n()) %>%

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de revocaciones",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label( # Etiquetas de las columnas
    CCAA = "Com. Aut.",
    SEXO = "Comunidad Autónoma",
    n = "Número de revocaciones"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab7_1, "tablas/tab7_1.html")

## Revocaciones por sexo (excel bruto)
tab7_2<-df %>%
  filter(Revocación == 1) %>% 
  group_by(SEXO) %>%
  summarise(n = n()) %>%
  mutate(SEXO = ifelse(SEXO == "H", "Hombre", "Mujer")) %>% # Cambiar etiquetas de sexo

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de revocaciones",
    subtitle = "Por Sexo"
  ) %>%
  cols_label( # Etiquetas de las columnas
    SEXO = "Sexo",
    n = "Número de revocaciones"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab7_2, "tablas/tab7_2.html")

## Revocaciones por enfermedad de base (excel bruto)
tab7_3<-df %>%
  filter(Revocación == 1) %>% 
  group_by(PATOLOGIA) %>%
  summarise(n = n()) %>%

  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de revocaciones",
    subtitle = "Por Enfermedad de Base"
  ) %>%
  cols_label( # Etiquetas de las columnas
    PATOLOGIA = "Enfermedad de Base",
    n = "Número de revocaciones"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab7_3, "tablas/tab7_3.html")

## Revocaciones por tramo de edad (excel bruto)
tab7_4<-df %>%
  filter(Revocación == 1) %>% 
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
    title = "Número de revocaciones",
    subtitle = "Por Tramo de Edad"
  ) %>%
  cols_label( # Etiquetas de las columnas
    TRAMO_EDAD = "Tramo de Edad",
    n = "Número de revocaciones"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab7_4, "tablas/tab7_4.html")

## Revocaciones por fase del proceso (excel bruto)
tab7_5<-df %>%
  filter(Revocación == 1) %>% 
  group_by(Tipo.de.revocación) %>%
  summarise(n = n()) %>%
 
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de revocaciones",
    subtitle = "Por Fase del Proceso"
  ) %>%
  cols_label( # Etiquetas de las columnas
    Tipo.de.revocación = "Fase del Proceso",
    n = "Número de revocaciones"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab7_5, "tablas/tab7_5.html")

## Tasa solicitudes/revocaciones (excel bruto)
tab7_6 <- df %>%
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



#8. PLAZOS Y PROCEDIMIENTOS DE LA TRAMITACIÓN
#8.1. ROL DE LOS PROFESIONALES SANITARIOS: MC Y MR
## Especialidad del MR y comunidad autónoma (excel 5)
especialidad_mr_df <- data.frame(
  Comunidad = c("ANDALUCÍA", "ARAGÓN", "ASTURIAS", "BALEARS", "CANARIAS", "CANTABRIA", 
                "CASTILLA-LA MANCHA", "CASTILLA Y LEÓN", "CATALUÑA", "COMUNITAT VALENCIANA", 
                "EXTREMADURA", "GALICIA", "MADRID", "MURCIA", "NAVARRA", "PAIS VASCO", 
                "RIOJA", "CEUTA", "MELILLA"),
  Geriatría = c(1,0,1,1,2,0,2,0,20,0,0,1,0,0,0,1,0,0,0),
  Medicina_Interna = c(0,0,0,3,1,0,1,2,15,2,0,3,3,0,0,2,0,0,0),
  Medicina_de_Familia = c(47,12,6,35,21,9,10,20,262,20,4,15,95,4,17,8,7,0,0),
  Neurología = c(2,3,3,0,16,0,0,5,17,0,0,4,9,0,1,20,2,0,0),
  Oncología = c(0,4,0,1,3,1,0,0,14,3,0,1,17,0,4,18,0,0,0),
  Otra = c(2,0,3,1,10,2,3,3,28,1,3,2,6,1,1,19,0,0,0)
)

tab8_1_1 <- especialidad_mr_df %>%
  gt() %>%
  tab_header(
    title = "Especialidad del Médico Responsable (MR)",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label(
    Comunidad = "Com. Aut.",
    Geriatría = "Geriatría",
    Medicina_Interna = "Medicina Interna",
    Medicina_de_Familia = "Medicina de Familia",
    Neurología = "Neurología",
    Oncología = "Oncología",
    Otra = "Otras especialidades"
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia a partir de datos autonómicos sobre objeción de conciencia a la eutanasia."
  )

gtsave(tab8_1_1, "tablas/tab8_1_1.html")

## Especialidad del MC y comunidad autónoma (excel 7)
especialidad_mc_df <- data.frame(
  Comunidad = c("ANDALUCÍA", "ARAGÓN", "ASTURIAS", "BALEARS", "CANARIAS", "CANTABRIA", 
                "CASTILLA-LA MANCHA", "CASTILLA Y LEÓN", "CATALUÑA", "COMUNITAT VALENCIANA", 
                "EXTREMADURA", "GALICIA", "MADRID", "MURCIA", "NAVARRA", "PAIS VASCO", 
                "RIOJA", "CEUTA", "MELILLA"),
  Geriatría = c(0,0,0,0,4,0,1,0,16,0,0,0,0,0,1,3,0,0,0),
  Medicina_Interna = c(4,2,0,2,5,0,1,4,13,4,1,1,7,0,1,5,1,0,0),
  Medicina_de_Familia = c(15,0,2,0,5,0,0,1,77,0,1,0,53,1,4,36,2,0,0),
  Neurología = c(15,6,4,9,10,2,4,9,54,4,2,7,4,3,4,2,4,0,0),
  Oncología = c(3,5,0,9,2,1,0,1,31,5,2,3,8,0,9,8,0,0,0),
  Otra = c(9,1,2,6,10,3,4,4,16,8,0,4,11,0,4,20,1,0,0)
)

tab8_1_2 <- especialidad_mc_df %>%
  gt() %>%
  tab_header(
    title = "Especialidad del Médico Consultor (MC)",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label(
    Comunidad = "Com. Aut.",
    Geriatría = "Geriatría",
    Medicina_Interna = "Medicina Interna",
    Medicina_de_Familia = "Medicina de Familia",
    Neurología = "Neurología",
    Oncología = "Oncología",
    Otra = "Otras especialidades"
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia a partir de datos autonómicos sobre objeción de conciencia a la eutanasia."
  )

gtsave(tab8_1_2, "tablas/tab8_1_2.html")

#8.2. DATOS SOBRE OBJECIÓN DE CONCIENCIA

#8.3. TIEMPOS DEL PROCEDIMIENTO
## Media y mediana de tiempo entre 1ª solicitud hasta que se autoriza la prestación por parte de la CGyE por años 2021-24 (excel 15+19).
prim_sol_autorizacion_df <- data.frame(
  Comunidad = c("ANDALUCÍA", "ARAGÓN", "ASTURIAS", "BALEARS", "CANARIAS", "CANTABRIA",
                "CASTILLA-LA MANCHA", "CASTILLA Y LEÓN", "CATALUÑA", "COMUNITAT VALENCIANA",
                "EXTREMADURA", "GALICIA", "MADRID", "MURCIA", "NAVARRA", "PAIS VASCO",
                "RIOJA", "CEUTA", "MELILLA"),
  Media_días = c(115, 70, 63, 71.3, 81, 42.3, 79.3, 103, 63, 91.8, 68.5, 67, 87, 43, 52, 49.2, 43, 0, 0),
  Mediana_días = c(90, 48.5, 57, 53, 59, 48, 67, 80, 50, 66, 53, 55.5, 71.5, 46, 40.5, 50, 44.5, 0, 0)
)

tab8_3_1 <- prim_sol_autorizacion_df %>%
  gt() %>%
  tab_header(
    title = "Tiempo transcurrido desde la primera solicitud hasta la autorización de la prestación por parte de la CGyE",
    subtitle = "Media y mediana de días"
  ) %>%
  cols_label(
    Comunidad = "Com. Aut.",
    Media_días = "Media (días)",
    Mediana_días = "Mediana (días)"
  ) %>%
  fmt_number(
    columns = c(Media_días, Mediana_días),
    decimals = 2
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia a partir de datos autonómicos sobre la Ley Orgánica de regulación de la eutanasia."
  )

gtsave(tab8_3_1, "tablas/tab8_3_1.html")

## Media y mediana de tiempo entre 1ª solicitud y 2ª solicitud por años 2021-24 (excel 16)
prim_sol_seg_sol_df <- data.frame(
  Comunidad = c("ANDALUCÍA", "ARAGÓN", "ASTURIAS", "BALEARS", "CANARIAS", "CANTABRIA",
                "CASTILLA-LA MANCHA", "CASTILLA Y LEÓN", "CATALUÑA", "COMUNITAT VALENCIANA",
                "EXTREMADURA", "GALICIA", "MADRID", "MURCIA", "NAVARRA", "PAIS VASCO",
                "RIOJA", "CEUTA", "MELILLA"),
  Media_días = c(26.23, 20, 27, 23, 28, 15, 30.7, 31, 20, 20, 15.5, 23, 27, 27.6, 15, 22, 17.5, 0, 0),
  Mediana_días = c(18.5, 15, 19, 16, 19, 15, 19.5, 22, 16, 17, 15, 16, 16, 15, 15, 15, 17.5, 0, 0)
)

tab8_3_2 <- prim_sol_seg_sol_df %>%
  gt() %>%
  tab_header(
    title = "Tiempo transcurrido desde la primera solicitud hasta la segunda solicitud",
    subtitle = "Media y mediana de días"
  ) %>%
  cols_label(
    Comunidad = "Com. Aut.",
    Media_días = "Media (días)",
    Mediana_días = "Mediana (días)"
  ) %>%
  fmt_number(
    columns = c(Media_días, Mediana_días),
    decimals = 2
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia a partir de datos autonómicos sobre la Ley Orgánica de regulación de la eutanasia."
  )

gtsave(tab8_3_2, "tablas/tab8_3_2.html")

## Media y mediana de tiempo entre 2ª solicitud e informe MC por años 2021-24 (excel 17) .
seg_sol_mc_df <- data.frame(
  Comunidad = c("ANDALUCÍA", "ARAGÓN", "ASTURIAS", "BALEARS", "CANARIAS", "CANTABRIA",
                "CASTILLA-LA MANCHA", "CASTILLA Y LEÓN", "CATALUÑA", "COMUNITAT VALENCIANA",
                "EXTREMADURA", "GALICIA", "MADRID", "MURCIA", "NAVARRA", "PAIS VASCO",
                "RIOJA", "CEUTA", "MELILLA"),
  Media_días = c(2, 9, 5, 19, 8, 10, 17, 17.6, 0, 10, 10, 11, 17, 4.4, 5.5, 6.7, 10, 0, 0),
  Mediana_días = c(14, 4, 4.5, 10.5, 6, 8, 10.5, 10, 0, 7, 10.5, 12, 10, 4, 4, 5, 10.5, 0, 0)
)

tab8_3_3 <- seg_sol_mc_df %>%
  gt() %>%
  tab_header(
    title = "Tiempo transcurrido desde la segunda solicitud hasta el informe favorable del médico consultor (MC)",
    subtitle = "Media y mediana de días"
  ) %>%
  cols_label(
    Comunidad = "Com. Aut.",
    Media_días = "Media (días)",
    Mediana_días = "Mediana (días)"
  ) %>%
  fmt_number(
    columns = c(Media_días, Mediana_días),
    decimals = 2
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia a partir de datos autonómicos sobre la Ley Orgánica de regulación de la eutanasia."
  )

gtsave(tab8_3_3, "tablas/tab8_3_3.html")

## Media y mediana de tiempo entre informe MC y resolución de CGyE por años 2021-24 (excel 18).
mc_cgye_df <- data.frame(
Comunidad = c("ANDALUCÍA", "ARAGÓN", "ASTURIAS", "BALEARS", "CANARIAS", "CANTABRIA",
                "CASTILLA-LA MANCHA", "CASTILLA Y LEÓN", "CATALUÑA", "COMUNITAT VALENCIANA",
                "EXTREMADURA", "GALICIA", "MADRID", "MURCIA", "NAVARRA", "PAIS VASCO",
                "RIOJA", "CEUTA", "MELILLA"),
  Media_días = c(17, 9, 8, 15.7, 17, 9.6, 15.25, 9, 0, 13, 24.5, 21, 13, 12, 6, 8.5, 19, 0, 0),
  Mediana_días = c(14, 9, 6, 11, 13.5, 10, 14.5, 7.5, 0, 11, 19, 10, 10, 9.5, 4, 8.5, 7, 0, 0)
)

tab8_3_4 <- mc_cgye_df %>%
  gt() %>%
  tab_header(
    title = "Tiempo transcurrido desde el informe favorable del médico consultor (MC) hasta la resolución de la CGyE",
    subtitle = "Media y mediana de días"
  ) %>%
  cols_label(
    Comunidad = "Com. Aut.",
    Media_días = "Media (días)",
    Mediana_días = "Mediana (días)"
  ) %>%
  fmt_number(
    columns = c(Media_días, Mediana_días),
    decimals = 2
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia a partir de datos autonómicos sobre la Ley Orgánica de regulación de la eutanasia."
  )

gtsave(tab8_3_4, "tablas/tab8_3_4.html")

## Media y mediana de tiempo entre resolución de CGyE y la prestación por años 2021-24 (excel 19).
cgye_prest_df <- data.frame(
 Comunidad = c("ANDALUCÍA", "ARAGÓN", "ASTURIAS", "BALEARS", "CANARIAS", "CANTABRIA",
                "CASTILLA-LA MANCHA", "CASTILLA Y LEÓN", "CATALUÑA", "COMUNITAT VALENCIANA",
                "EXTREMADURA", "GALICIA", "MADRID", "MURCIA", "NAVARRA", "PAIS VASCO",
                "RIOJA", "CEUTA", "MELILLA"),
  Media_días = c(46, 12.5, 19.4, 14, 26, 12, 22, 48, 0, 25, 15.75, 15, 31, 15, 25, 17.4, 33, 0, 0),
  Mediana_días = c(24, 11, 14, 12, 15, 13, 18, 28, 0, 13, 5, 14.5, 19, 16, 11, 14, 9.5, 0, 0)
)

tab8_3_5 <- cgye_prest_df %>%
  gt() %>%
  tab_header(
    title = "Tiempo transcurrido desde la resolución favorable de la CGyE y la realización de la prestación",
    subtitle = "Media y mediana de días"
  ) %>%
  cols_label(
    Comunidad = "Com. Aut.",
    Media_días = "Media (días)",
    Mediana_días = "Mediana (días)"
  ) %>%
  fmt_number(
    columns = c(Media_días, Mediana_días),
    decimals = 2
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia a partir de datos autonómicos sobre la Ley Orgánica de regulación de la eutanasia."
  )

gtsave(tab8_3_5, "tablas/tab8_3_5.html")

## Media y mediana de tiempo entre la reclamación a la CGyE y la respuesta por años 2021-24 (excel 20).
reclam_resol_df <- data.frame(
Comunidad = c("ANDALUCÍA", "ARAGÓN", "ASTURIAS", "BALEARS", "CANARIAS", "CANTABRIA",
                "CASTILLA-LA MANCHA", "CASTILLA Y LEÓN", "CATALUÑA", "COMUNITAT VALENCIANA",
                "EXTREMADURA", "GALICIA", "MADRID", "MURCIA", "NAVARRA", "PAIS VASCO",
                "RIOJA", "CEUTA", "MELILLA"),
  Media_días = c(22, 14.6, 25, 33, 0, 13, 0, 0, 0, 43, 0, 20, 15.7, 12.5, 0, 0, 0, 0, 0),
  Mediana_días = c(20, 13, 20, 18.5, 0, 12, 0, 0, 0, 15, 0, 20, 14, 12.5, 0, 0, 0, 0, 0)
)

tab8_3_6 <- reclam_resol_df %>%
  gt() %>%
  tab_header(
    title = "Tiempo transcurrido desde la reclamación de la persona solicitante hasta la resolución de la CGyE",
    subtitle = "Media y mediana de días"
  ) %>%
  cols_label(
    Comunidad = "Com. Aut.",
    Media_días = "Media (días)",
    Mediana_días = "Mediana (días)"
  ) %>%
  fmt_number(
    columns = c(Media_días, Mediana_días),
    decimals = 2
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia a partir de datos autonómicos sobre la Ley Orgánica de regulación de la eutanasia."
  )

gtsave(tab8_3_6, "tablas/tab8_3_6.html")

#8.4. APLAZAMIENTOS DE LA PRESTACIÓN
## Aplazamientos por comunidad autónoma (excel bruto)
tab8_4_1<-df %>%
  filter(Aplazamiento == 1) %>% 
  group_by(CCAA) %>%
  summarise(n = n()) %>%
 
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de aplazamientos",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label( # Etiquetas de las columnas
    CCAA = "Com. Aut.",
    n = "Número de aplazamientos"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab8_4_1, "tablas/tab8_4_1.html")

## Aplazamientos por años 2021-24 (excel bruto) 
df_aplazamientos_año <- data.frame(
Año = c(2021, 2022, 2023, 2024),
n_aplazamientos = c(21, 22, 23, 24)
)

tab8_4_2<-df_aplazamientos_año %>%

gt() %>%
tab_header( # Añadir título y subtítulo
title = "Número de aplazamientos",
subtitle = "2021-2024"
) %>%
cols_label( # Etiquetas de las columnas
n_aplazamientos = "Número de aplazamientos"
) %>%
tab_source_note( # Añadir nota de la fuente
source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
)

gtsave(tab8_4_2, "tablas/tab8_4_2.html")


## Aplazamientos por tramos de edad (excel bruto)
tab8_4_3<-df %>%
  filter(Aplazamiento == 1) %>% 
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
    title = "Número de aplazamientos",
    subtitle = "Por Tramo de Edad"
  ) %>%
  cols_label( # Etiquetas de las columnas
    TRAMO_EDAD = "Tramo de Edad",
    n = "Número de aplazamientos"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab8_4_3, "tablas/tab8_4_3.html")

## Aplazamientos por sexo (excel bruto)
tab8_4_4<-df %>%
  filter(Aplazamiento == 1) %>% 
  group_by(SEXO) %>%
  summarise(n = n()) %>%
  mutate(SEXO = ifelse(SEXO == "H", "Hombre", "Mujer")) %>% # Cambiar etiquetas de sexo
 
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de aplazamientos",
    subtitle = "Por Sexo"
  ) %>%
  cols_label( # Etiquetas de las columnas
    SEXO = "Sexo",
    n = "Número de aplazamientos"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab8_4_4, "tablas/tab8_4_4.html")

## Aplazamientos por enfermedad de base (excel bruto)
tab8_4_5<-df %>%
  filter(Aplazamiento == 1) %>% 
  group_by(PATOLOGIA) %>%
  summarise(n = n()) %>%
   
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de aplazamientos",
    subtitle = "Por Enfermedad de Base"
  ) %>%
  cols_label( # Etiquetas de las columnas
    PATOLOGIA = "Enfermedad de Base",
    n = "Número de aplazamientos"
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab8_4_5, "tablas/tab8_4_5.html")

## Tasas de aplazamientos por solicitud aceptada por CGyE (excel bruto)
tab8_4_6 <- df %>%
  group_by(CCAA) %>%
  summarise(
    total_solicitudes_CGyE = sum(Informe.favorable.Comisión.Garantía.y.Evaluación..CGyE. == 1, na.rm = TRUE),
    total_aplazamientos = sum(Aplazamiento == 1, na.rm = TRUE)
    ) %>%
  mutate(
    tasa_aplazamientos = round((total_aplazamientos / total_solicitudes_CGyE) * 100, 2)
  ) %>%

  gt() %>%
  tab_header(
    title = "Tasas por Solicitudes Aceptadas por la CGyE",
    subtitle = "Aplazamientos sobre solicitudes"
  ) %>%
  cols_label(
    CCAA = "Com. Aut.",
    total_solicitudes_CGyE = "Solicitudes aceptadas CGyE",
    total_aplazamientos = "Aplazamientos",
    tasa_aplazamientos = "Tasa aplazamientos (%)"
    ) %>%
  tab_source_note(
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab8_4_6, "tablas/tab8_4_6.html")

## Tasa de aplazamientos por solicitudes totales por C.A (excel bruto)
tab8_4_7 <- df %>%
  group_by(CCAA) %>%
  summarise(
    total_solicitudes = n(),
    total_aplazamientos = sum(Aplazamiento == 1, na.rm = TRUE)
    ) %>%
  mutate(
    tasa_aplazamientos = round((total_aplazamientos / total_solicitudes) * 100, 2)
  ) %>%

  gt() %>%
  tab_header(
    title = "Tasas por Comunidad Autónoma",
    subtitle = "Aplazamientos sobre solicitudes"
  ) %>%
  cols_label(
    CCAA = "Com. Aut.",
    total_solicitudes = "Solicitudes",
    total_aplazamientos = "Aplazamientos",
    tasa_aplazamientos = "Tasa aplazamientos (%)"
    ) %>%
  tab_source_note(
    source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
  ) 

gtsave(tab8_4_7, "tablas/tab8_4_7.html")

#9. ESTADÍSTICAS RELACIONADAS CON LA DONACIÓN DE ÓRGANOS POST-PAM (sintetizar de forma vistosa)
## Frecuencia de donantes tras la PAM por años 2021-24 por C.A. (excel 21).
df_donantes_año <- data.frame(
Año = c(2021, 2022, 2023, 2024),
n_donantes = c(21, 22, 23, 24)
)

tab9_1<-df_donantes_año %>%

gt() %>%
tab_header( # Añadir título y subtítulo
title = "Número de donantes",
subtitle = "2021-2024"
) %>%
cols_label( # Etiquetas de las columnas
n_donantes = "Número de donantes"
) %>%
tab_source_note( # Añadir nota de la fuente
source_note = "Fuente: Datos simulados para el análisis de solicitudes de PAM."
)

gtsave(tab9_1, "tablas/tab9_1.html")

## Frecuencia de órganos extraídos tras la PAM por años 2021-24 por C.A. (disponible en borrador).????
## Frecuencia de órganos trasplantados tras la PAM por años 2021-24 por C.A. (disponible en borrador).????
## Frecuencia de personas trasplantadas tras la PAM por años 2021-24 por C.A. (disponible en borrador).????
