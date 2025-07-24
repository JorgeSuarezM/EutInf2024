#7. REVOCACIONES DE LA PAM
## Revocaciones por comunidad autónoma (excel bruto)
# Tabla por CCAA
tab7_1_ccaa <- df %>%
  filter(Revocación == 1) %>%
  group_by(CCAA) %>%
  summarise(n = n())

# Fila total
tab7_1_total <- tab7_1_ccaa %>%
  summarise(CCAA = "Total", n = sum(n))

# Unir tabla y total
tab7_1_final <- bind_rows(tab7_1_ccaa, tab7_1_total)

# Crear tabla gt
tab7_1_gt <- tab7_1_final %>%
  gt() %>%
  tab_header(
    title = "Número de revocaciones",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label(
    CCAA = "Com. Aut.",
    n = "Número de revocaciones"
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

gtsave(tab7_1_gt, "tablas/tab7_1.html")
## Revocaciones por sexo (excel bruto)
tab7_2_sexo <- df %>%
  filter(Revocación == 1) %>%
  group_by(SEXO) %>%
  summarise(n = n()) %>%
  mutate(SEXO = ifelse(SEXO == "H", "Hombre", "Mujer"))

# Fila total
tab7_2_total <- tab7_2_sexo %>%
  summarise(SEXO = "Total", n = sum(n))

# Unir tabla y total
tab7_2_final <- bind_rows(tab7_2_sexo, tab7_2_total)

# Crear tabla gt
tab7_2_gt <- tab7_2_final %>%
  gt() %>%
  tab_header(
    title = "Número de revocaciones",
    subtitle = "Por Sexo"
  ) %>%
  cols_label(
    SEXO = "Sexo",
    n = "Número de revocaciones"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = SEXO == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab7_2_gt, "tablas/tab7_2.html")

## Revocaciones por enfermedad de base (excel bruto)
tab7_3_patologia <- df %>%
  filter(Revocación == 1) %>%
  group_by(PATOLOGIA) %>%
  summarise(n = n())

# Fila total
tab7_3_total <- tab7_3_patologia %>%
  summarise(PATOLOGIA = "Total", n = sum(n))

# Unir tabla y total
tab7_3_final <- bind_rows(tab7_3_patologia, tab7_3_total)

# Crear tabla gt
tab7_3_gt <- tab7_3_final %>%
  gt() %>%
  tab_header(
    title = "Número de revocaciones",
    subtitle = "Por Enfermedad de Base"
  ) %>%
  cols_label(
    PATOLOGIA = "Enfermedad de Base",
    n = "Número de revocaciones"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = PATOLOGIA == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab7_3_gt, "tablas/tab7_3.html")

## Revocaciones por tramo de edad (excel bruto)
tab7_4_tramo <- df %>%
  filter(Revocación == 1) %>%
  group_by(TRAMO_EDAD) %>%
  summarise(n = n()) %>%
  mutate(TRAMO_EDAD = case_when(
    TRAMO_EDAD == "1-Tramo de edad (<30)" ~ "Menores de 30 años",
    TRAMO_EDAD == "2-Tramo de edad (30-39)" ~ "Entre 30 y 39 años",
    TRAMO_EDAD == "3-Tramo de edad (40-49)" ~ "Entre 40 y 49 años",
    TRAMO_EDAD == "4-Tramo de edad (50-59)" ~ "Entre 50 y 59 años",
    TRAMO_EDAD == "5-Tramo de edad (60-69)" ~ "Entre 60 y 69 años",
    TRAMO_EDAD == "6-Tramo de edad (70-79)" ~ "Entre 70 y 79 años",
    TRAMO_EDAD == "7-Tramo de edad (> 80)" ~ "Mayores de 80 años",
    TRUE ~ TRAMO_EDAD
  ))

# Fila total
tab7_4_total <- tab7_4_tramo %>%
  summarise(TRAMO_EDAD = "Total", n = sum(n))

# Unir tabla y total
tab7_4_final <- bind_rows(tab7_4_tramo, tab7_4_total)

# Crear tabla gt
tab7_4_gt <- tab7_4_final %>%
  gt() %>%
  tab_header(
    title = "Número de revocaciones",
    subtitle = "Por Tramo de Edad"
  ) %>%
  cols_label(
    TRAMO_EDAD = "Tramo de Edad",
    n = "Número de revocaciones"
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

gtsave(tab7_4_gt, "tablas/tab7_4.html")

## Revocaciones por fase del proceso (excel bruto)
tab7_5_fase <- df %>%
  filter(Revocación == 1) %>%
  group_by(Tipo.de.revocación) %>%
  summarise(n = n())

# Fila total
tab7_5_total <- tab7_5_fase %>%
  summarise(Tipo.de.revocación = "Total", n = sum(n))

# Unir tabla y total
tab7_5_final <- bind_rows(tab7_5_fase, tab7_5_total)

# Crear tabla gt
tab7_5_gt <- tab7_5_final %>%
  gt() %>%
  tab_header(
    title = "Número de revocaciones",
    subtitle = "Por Fase del Proceso"
  ) %>%
  cols_label(
    Tipo.de.revocación = "Fase del Proceso",
    n = "Número de revocaciones"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Tipo.de.revocación == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab7_5_gt, "tablas/tab7_5.html")

## Tasa solicitudes/revocaciones (excel bruto)
tab7_6 <- df %>%
  mutate(
    CCAA = trimws(CCAA),
    CCAA = toupper(CCAA),
    CCAA = ifelse(is.na(CCAA) | CCAA == "" | CCAA == "PERDIDOS", "Desconocido", CCAA)
  ) %>%
  group_by(CCAA) %>%
  summarise(
    total_solicitudes = n(),
    total_revocaciones = sum(Revocación == 1, na.rm = TRUE)
    ) %>%
  mutate(
    tasa_revocaciones = round((total_revocaciones / total_solicitudes) * 100, 2)
  ) %>%
  ungroup() %>%
  bind_rows(
    tibble(
      CCAA = "Total",
      total_solicitudes = sum(.$total_solicitudes),
      total_revocaciones = sum(.$total_revocaciones),
      tasa_revocaciones = round((sum(.$total_revocaciones) / sum(.$total_solicitudes)) * 100, 2)
    )
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
# Calcular la fila total
total_row <- especialidad_mr_df %>%
  summarise(
    Comunidad = "Total",
    Geriatría = sum(Geriatría),
    Medicina_Interna = sum(Medicina_Interna),
    Medicina_de_Familia = sum(Medicina_de_Familia),
    Neurología = sum(Neurología),
    Oncología = sum(Oncología),
    Otra = sum(Otra)
  )

# Unir la fila total a la tabla original
especialidad_mr_df_total <- bind_rows(especialidad_mr_df, total_row)

# Crear la tabla gt
tab8_1_1 <- especialidad_mr_df_total %>%
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
    source_note = "Fuente: Elaboración propia."
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

# Calcular la fila total
total_row <- especialidad_mc_df %>%
  summarise(
    Comunidad = "Total",
    Geriatría = sum(Geriatría),
    Medicina_Interna = sum(Medicina_Interna),
    Medicina_de_Familia = sum(Medicina_de_Familia),
    Neurología = sum(Neurología),
    Oncología = sum(Oncología),
    Otra = sum(Otra)
  )

# Unir la fila total a la tabla original
especialidad_mc_df_total <- bind_rows(especialidad_mc_df, total_row)

# Crear la tabla gt
tab8_1_2 <- especialidad_mc_df_total %>%
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
    source_note = "Fuente: Elaboración propia"
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

# Filtrar datos para excluir Ceuta y Melilla (valores 0) para el cálculo del total nacional
datos_ccaa <- prim_sol_autorizacion_df %>%
  filter(Media_días > 0 & Mediana_días > 0)

# Calcular media y mediana nacional
media_nacional <- round(mean(datos_ccaa$Media_días), 1)
mediana_nacional <- round(median(datos_ccaa$Mediana_días), 1)

# Añadir fila de Total Nacional
prim_sol_autorizacion_df <- prim_sol_autorizacion_df %>%
  rbind(data.frame(
    Comunidad = "TOTAL NACIONAL",
    Media_días = media_nacional,
    Mediana_días = mediana_nacional
  ))

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
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Comunidad == "TOTAL NACIONAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
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

# Filtrar datos para excluir Ceuta y Melilla (valores 0) para el cálculo del total nacional
datos_ccaa_2 <- prim_sol_seg_sol_df %>%
  filter(Media_días > 0 & Mediana_días > 0)

# Calcular media y mediana nacional
media_nacional_2 <- round(mean(datos_ccaa_2$Media_días), 1)
mediana_nacional_2 <- round(median(datos_ccaa_2$Mediana_días), 1)

# Añadir fila de Total Nacional
prim_sol_seg_sol_df <- prim_sol_seg_sol_df %>%
  rbind(data.frame(
    Comunidad = "TOTAL NACIONAL",
    Media_días = media_nacional_2,
    Mediana_días = mediana_nacional_2
  ))

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
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Comunidad == "TOTAL NACIONAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
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

# Filtrar datos para excluir Ceuta, Melilla y Cataluña (valores 0) para el cálculo del total nacional
datos_ccaa_3 <- seg_sol_mc_df %>%
  filter(Media_días > 0 & Mediana_días > 0)

# Calcular media y mediana nacional
media_nacional_3 <- round(mean(datos_ccaa_3$Media_días), 1)
mediana_nacional_3 <- round(median(datos_ccaa_3$Mediana_días), 1)

# Añadir fila de Total Nacional
seg_sol_mc_df <- seg_sol_mc_df %>%
  rbind(data.frame(
    Comunidad = "TOTAL NACIONAL",
    Media_días = media_nacional_3,
    Mediana_días = mediana_nacional_3
  ))

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
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Comunidad == "TOTAL NACIONAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
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

# Filtrar datos para excluir Ceuta, Melilla y Cataluña (valores 0) para el cálculo del total nacional
datos_ccaa_4 <- mc_cgye_df %>%
  filter(Media_días > 0 & Mediana_días > 0)

# Calcular media y mediana nacional
media_nacional_4 <- round(mean(datos_ccaa_4$Media_días), 1)
mediana_nacional_4 <- round(median(datos_ccaa_4$Mediana_días), 1)

# Añadir fila de Total Nacional
mc_cgye_df <- mc_cgye_df %>%
  rbind(data.frame(
    Comunidad = "TOTAL NACIONAL",
    Media_días = media_nacional_4,
    Mediana_días = mediana_nacional_4
  ))

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
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Comunidad == "TOTAL NACIONAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
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

# Filtrar datos para excluir Ceuta, Melilla y Cataluña (valores 0) para el cálculo del total nacional
datos_ccaa_5 <- cgye_prest_df %>%
  filter(Media_días > 0 & Mediana_días > 0)

# Calcular media y mediana nacional
media_nacional_5 <- round(mean(datos_ccaa_5$Media_días), 1)
mediana_nacional_5 <- round(median(datos_ccaa_5$Mediana_días), 1)

# Añadir fila de Total Nacional
cgye_prest_df <- cgye_prest_df %>%
  rbind(data.frame(
    Comunidad = "TOTAL NACIONAL",
    Media_días = media_nacional_5,
    Mediana_días = mediana_nacional_5
  ))

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
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Comunidad == "TOTAL NACIONAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
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

# Filtrar datos para excluir comunidades con valores 0 para el cálculo del total nacional
datos_ccaa_6 <- reclam_resol_df %>%
  filter(Media_días > 0 & Mediana_días > 0)

# Calcular media y mediana nacional
media_nacional_6 <- round(mean(datos_ccaa_6$Media_días), 1)
mediana_nacional_6 <- round(median(datos_ccaa_6$Mediana_días), 1)

# Añadir fila de Total Nacional
reclam_resol_df <- reclam_resol_df %>%
  rbind(data.frame(
    Comunidad = "TOTAL NACIONAL",
    Media_días = media_nacional_6,
    Mediana_días = mediana_nacional_6
  ))

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
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Comunidad == "TOTAL NACIONAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab8_3_6, "tablas/tab8_3_6.html")

#8.4. APLAZAMIENTOS DE LA PRESTACIÓN
## Aplazamientos por comunidad autónoma (excel bruto)
# Tabla por CCAA
tab8_4_1_ccaa <- df %>%
  filter(Aplazamiento == 1) %>% 
  group_by(CCAA) %>%
  summarise(n = n())

# Fila total
tab8_4_1_total <- tab8_4_1_ccaa %>%
  summarise(CCAA = "Total", n = sum(n))

# Unir tabla y total
tab8_4_1_final <- bind_rows(tab8_4_1_ccaa, tab8_4_1_total)

# Crear tabla gt
tab8_4_1 <- tab8_4_1_final %>%
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de aplazamientos",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label( # Etiquetas de las columnas
    CCAA = "Com. Aut.",
    n = "Número de aplazamientos"
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
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab8_4_1, "tablas/tab8_4_1.html")

## Aplazamientos por años 2021-24 (excel bruto) 
# Tabla por año
df_aplazamientos_año <- data.frame(
Año = c(2021, 2022, 2023, 2024),
n_aplazamientos = c(6, 22, 33, 64)
) %>%
  mutate(Año = as.character(Año))

# Fila total
total_aplazamientos_año <- df_aplazamientos_año %>%
  summarise(Año = "Total", n_aplazamientos = sum(n_aplazamientos))

# Unir tabla y total
df_aplazamientos_año_final <- bind_rows(df_aplazamientos_año, total_aplazamientos_año)

# Crear tabla gt
tab8_4_2 <- df_aplazamientos_año_final %>%
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de aplazamientos",
    subtitle = "2021-2024"
  ) %>%
  cols_label( # Etiquetas de las columnas
    Año = "Año",
    n_aplazamientos = "Número de aplazamientos"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Año == "Total"
    )
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab8_4_2, "tablas/tab8_4_2.html")


## Aplazamientos por tramos de edad (excel bruto)
aplazamientos_tramo_df <- df %>%
  filter(Aplazamiento == 1) %>% 
  group_by(TRAMO_EDAD) %>%
  summarise(n = n()) %>%
  mutate(TRAMO_EDAD = ifelse(TRAMO_EDAD == "1-Tramo de edad (<30)", "Menores de 30 años",
                             ifelse(TRAMO_EDAD == "2-Tramo de edad (30-39)", "Entre 30 y 39 años",
                                ifelse(TRAMO_EDAD == "3-Tramo de edad (40-49)", "Entre 40 y 49 años",
                                    ifelse(TRAMO_EDAD == "4-Tramo de edad (50-59)", "Entre 50 y 59 años",
                                        ifelse(TRAMO_EDAD == "4-Tramo de edad (60-69)", "Entre 60 y 69 años",
                                            ifelse(TRAMO_EDAD == "5-Tramo de edad (70-79)", "Entre 70 y 79 años",
                                                ifelse(TRAMO_EDAD == "6-Tramo de edad (> 80)", "Mayores de 80 años", TRAMO_EDAD))))))))

# Fila total
total_aplazamientos_tramo <- data.frame(
  TRAMO_EDAD = "Total",
  n = sum(aplazamientos_tramo_df$n)
)

# Unir tabla y total
aplazamientos_tramo_final <- rbind(aplazamientos_tramo_df, total_aplazamientos_tramo)

tab8_4_3 <- aplazamientos_tramo_final %>%
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de aplazamientos",
    subtitle = "Por Tramo de Edad"
  ) %>%
  cols_label( # Etiquetas de las columnas
    TRAMO_EDAD = "Tramo de Edad",
    n = "Número de aplazamientos"
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
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab8_4_3, "tablas/tab8_4_3.html")

## Aplazamientos por sexo (excel bruto)
aplazamientos_sexo_df <- df %>%
  filter(Aplazamiento == 1) %>% 
  group_by(SEXO) %>%
  summarise(n = n()) %>%
  mutate(SEXO = ifelse(SEXO == "H", "Hombre", "Mujer")) # Cambiar etiquetas de sexo

# Fila total
total_aplazamientos_sexo <- data.frame(
  SEXO = "Total",
  n = sum(aplazamientos_sexo_df$n)
)

# Unir tabla y total
aplazamientos_sexo_final <- rbind(aplazamientos_sexo_df, total_aplazamientos_sexo)

tab8_4_4 <- aplazamientos_sexo_final %>%
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de aplazamientos",
    subtitle = "Por Sexo"
  ) %>%
  cols_label( # Etiquetas de las columnas
    SEXO = "Sexo",
    n = "Número de aplazamientos"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = SEXO == "Total"
    )
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab8_4_4, "tablas/tab8_4_4.html")

## Aplazamientos por enfermedad de base (excel bruto)
aplazamientos_patologia_df <- df %>%
  filter(Aplazamiento == 1) %>% 
  group_by(PATOLOGIA) %>%
  summarise(n = n())

# Fila total
total_aplazamientos_patologia <- data.frame(
  PATOLOGIA = "Total",
  n = sum(aplazamientos_patologia_df$n)
)

# Unir tabla y total
aplazamientos_patologia_final <- rbind(aplazamientos_patologia_df, total_aplazamientos_patologia)

tab8_4_5 <- aplazamientos_patologia_final %>%
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de aplazamientos",
    subtitle = "Por Enfermedad de Base"
  ) %>%
  cols_label( # Etiquetas de las columnas
    PATOLOGIA = "Enfermedad de Base",
    n = "Número de aplazamientos"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = PATOLOGIA == "Total"
    )
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab8_4_5, "tablas/tab8_4_5.html")

## Tasas de aplazamientos por solicitud aceptada por CGyE (excel bruto)
tasas_aplazamientos_cgye_df <- df %>%
  group_by(CCAA) %>%
  summarise(
    total_solicitudes_CGyE = sum(InformeCGyE == 1, na.rm = TRUE),
    total_aplazamientos = sum(Aplazamiento == 1, na.rm = TRUE)
    ) %>%
  mutate(
    tasa_aplazamientos = round((total_aplazamientos / total_solicitudes_CGyE) * 100, 2)
  )

# Fila total
total_tasas_cgye <- tasas_aplazamientos_cgye_df %>%
  summarise(
    CCAA = "Total",
    total_solicitudes_CGyE = sum(total_solicitudes_CGyE),
    total_aplazamientos = sum(total_aplazamientos),
    tasa_aplazamientos = round((sum(total_aplazamientos) / sum(total_solicitudes_CGyE)) * 100, 2)
  )

# Unir tabla y total
tasas_cgye_final <- rbind(tasas_aplazamientos_cgye_df, total_tasas_cgye)

tab8_4_6 <- tasas_cgye_final %>%
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

gtsave(tab8_4_6, "tablas/tab8_4_6.html")

## Tasa de aplazamientos por solicitudes totales por C.A (excel bruto)
tasas_aplazamientos_ca_df <- df %>%
  group_by(CCAA) %>%
  summarise(
    total_solicitudes = n(),
    total_aplazamientos = sum(Aplazamiento == 1, na.rm = TRUE)
    ) %>%
  mutate(
    tasa_aplazamientos = round((total_aplazamientos / total_solicitudes) * 100, 2)
  )

# Fila total
total_tasas_ca <- tasas_aplazamientos_ca_df %>%
  summarise(
    CCAA = "Total",
    total_solicitudes = sum(total_solicitudes),
    total_aplazamientos = sum(total_aplazamientos),
    tasa_aplazamientos = round((sum(total_aplazamientos) / sum(total_solicitudes)) * 100, 2)
  )

# Unir tabla y total
tasas_ca_final <- rbind(tasas_aplazamientos_ca_df, total_tasas_ca)

tab8_4_7 <- tasas_ca_final %>%
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

gtsave(tab8_4_7, "tablas/tab8_4_7.html")


#9. ESTADÍSTICAS RELACIONADAS CON LA DONACIÓN DE ÓRGANOS POST-PAM (sintetizar de forma vistosa)
## Frecuencia de donantes tras la PAM por años 2021-24 (excel 21).
df_donantes_año <- data.frame(
  Año = c(2021, 2022, 2023, 2024),
  n_donantes = c(7, 42, 44, 63)
)

# Fila total
total_donantes <- data.frame(
  Año = "Total",
  n_donantes = sum(df_donantes_año$n_donantes)
)

# Unir tabla y total
df_donantes_año_final <- rbind(df_donantes_año, total_donantes)

tab9_1 <- df_donantes_año_final %>%
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de donantes",
    subtitle = "2021-2024"
  ) %>%
  cols_label( # Etiquetas de las columnas
    n_donantes = "Número de donantes"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Año == "Total"
    )
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab9_1, "tablas/tab9_1.html")

## Frecuencia de Donaciones, extracciones y trasplantes de órganos tras la PAM por años 2021-24 
df_donantes_organos_año <- data.frame(
  Año = c(2021, 2022, 2023, 2024),
  n_donantes = c(7, 42, 44, 63),
  n_pacientes_transplantados = c(22, 113, 115, 192),
  n_trasplantes_renales = c(13, 65, 67, 111),
  n_trasplantes_hepaticos = c(6, 27, 23, 40),
  n_trasplantes_cardiacos = c(0, 5, 5, 13),
  n_trasplantes_pulmonares = c(4, 19, 20, 28),
  n_trasplantes_pancreas = c(0, 5, 4, 4),
  n_organos_transplantados = c(23, 121, 119, 196)
)

# Transponer y calcular totales por fila (variable)
df_donantes_organos_año_t <- as.data.frame(t(df_donantes_organos_año[,-1]))
colnames(df_donantes_organos_año_t) <- as.character(df_donantes_organos_año$Año)
df_donantes_organos_año_t$`Total` <- rowSums(df_donantes_organos_año_t)
df_donantes_organos_año_t$Variable <- rownames(df_donantes_organos_año_t)
df_donantes_organos_año_t <- df_donantes_organos_año_t %>% dplyr::select(Variable, everything())

# Renombrar variables
nombres_variables <- c(
  n_donantes = "Donantes tras eutanasia",
  n_pacientes_transplantados = "Pacientes transplantados de donantes tras eutanasia",
  n_trasplantes_renales = "Trasplantes renales de donantes tras eutanasia",
  n_trasplantes_hepaticos = "Trasplantes hepáticos de donantes tras eutanasia",
  n_trasplantes_cardiacos = "Trasplantes cardíacos de donantes tras eutanasia",
  n_trasplantes_pulmonares = "Trasplantes pulmonares de donantes tras eutanasia",
  n_trasplantes_pancreas = "Trasplantes de páncreas de donantes tras eutanasia",
  n_organos_transplantados = "Órganos transplantados de donantes tras eutanasia"
)
df_donantes_organos_año_t$Variable <- nombres_variables[df_donantes_organos_año_t$Variable]

# Convertir columnas a numérico (excepto Variable)
for(col in colnames(df_donantes_organos_año_t)[-1]) {
  df_donantes_organos_año_t[[col]] <- as.numeric(df_donantes_organos_año_t[[col]])
}

# Crear tabla gt resaltando la última columna
tab9_2 <- df_donantes_organos_año_t %>%
  gt(rowname_col = "Variable") %>%
  tab_header(
    title = "Número de donantes, extracciones y trasplantes",
    subtitle = "2021-2024"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab9_2, "tablas/tab9_2.html")