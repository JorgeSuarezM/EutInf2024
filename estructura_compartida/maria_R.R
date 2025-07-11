##ESTRUCTURA DEL INFORME DE ESTADÍSTICAS DE LA PAM

#2. ESTADÍSTICAS GENERALES DE LA PAM
## Nº solicitudes por C.A 
tab2_1 <- df %>%
  mutate(
    CCAA = trimws(CCAA),
    CCAA = toupper(CCAA),
    CCAA = ifelse(is.na(CCAA) | CCAA == "" | CCAA == "PERDIDOS", "Desconocido", CCAA)
  ) %>%
  group_by(CCAA) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  bind_rows(
    tibble(CCAA = "Total", n = sum(.$n))
  ) %>%
  gt() %>%
  tab_header(title = "Nº de solicitudes de PAM por CCAA") %>%
  tab_spanner(label = "Resultados", columns = c(CCAA, n)) %>%
  cols_label(CCAA = "Comunidad Autónoma", n = "Número de solicitudes") %>%
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

gtsave(tab2_1, "tablas/tab2_1.html")

## Nº denegaciones por C.A No se pueden hacer

## Nº revocaciones por C.A Hecha en apartado de María B. Tabla 7_1

## Nº aplazamientos por C.A Hecha en apartado de María B. Tabla 8_4_1

## Nº fallecidos durante la tramitación por C.A
tab2_2<-df %>%
  filter(Fallecimiento.durante.tramitación == 1) %>% 
  mutate(
    CCAA = trimws(CCAA),
    CCAA = toupper(CCAA),
    CCAA = ifelse(is.na(CCAA) | CCAA == "" | CCAA == "PERDIDOS", "Desconocido", CCAA)
  ) %>%
  group_by(CCAA) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  bind_rows(
    tibble(CCAA = "Total", n = sum(.$n))
  ) %>%
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de fallecidos durante la tramitación de la PAM",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label( # Etiquetas de las columnas
    CCAA = "Com. Aut.",
    n = "Número de fallecidos durante la tramitación de la PAM"
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

gtsave(tab2_2, "tablas/tab2_2.html")

## Tasas de mortalidad por PAM (entre total fallecidos por C.A. y total nacional)
# 1. Defunciones totales por CCAA (nombres igual que en df, pero limpiados)
defunciones_CCAA <- tibble::tribble(
  ~CCAA, ~total_muertes,
  "Andalucía", 73485,
  "Aragón", 13928,
  "Asturias", 13137,
  "Illes Balears", 9003,
  "Canarias", 17786,
  "Cantabria", 6399,
  "Castilla y León", 29505,
  "Castilla-La Mancha", 20042,
  "Cataluña", 67505,
  "C. Valenciana", 46350,
  "Extremadura", 11924,
  "Galicia", 32716,
  "Madrid", 49974,
  "Murcia", 11856,
  "Navarra", 6016,
  "País Vasco", 22593,
  "La Rioja", 3249,
  "Ceuta", 541,
  "Melilla", 430
) %>%
  mutate(CCAA = case_when(
    CCAA == "Illes Balears" ~ "Illes Balears",
    CCAA == "C. Valenciana" ~ "C. Valenciana",
    TRUE ~ CCAA
  ))

# 2. Agrupa y cuenta prestaciones por PAM por CCAA (usando el nombre correcto de la columna y limpieza)
fallecidos_PAM_CCAA <- df %>%
  group_by(CCAA) %>%
  summarise(total_prestaciones = sum(!is.na(FechaPrestacion))) %>%
  ungroup()

# 3. Une ambos data frames asegurando que todas las CCAA estén presentes
tab2_4 <- defunciones_CCAA %>%
  left_join(fallecidos_PAM_CCAA, by = "CCAA") %>%
  mutate(
    total_prestaciones = ifelse(is.na(total_prestaciones), 0, total_prestaciones),
    tasa_mortalidad = round((total_prestaciones / total_muertes) * 100, 2)
  )

# 4. Añade fila total
tab2_3 <- tab2_4 %>%
  bind_rows(
    tibble(
      CCAA = "TOTAL",
      total_muertes = sum(tab2_4$total_muertes, na.rm = TRUE),
      total_prestaciones = sum(tab2_4$total_prestaciones, na.rm = TRUE),
      tasa_mortalidad = round((sum(tab2_4$total_prestaciones, na.rm = TRUE) / sum(tab2_4$total_muertes, na.rm = TRUE)) * 100, 2)
    )
  )

# 5. Tabla gt con fila total resaltada
tab2_3 %>%
  gt() %>%
  tab_header(
    title = "Tasa de mortalidad por PAM por Comunidad Autónoma",
    subtitle = "Porcentaje sobre defunciones registradas"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    total_prestaciones = "Total prestaciones",
    total_muertes = "Defunciones registradas",
    tasa_mortalidad = "Tasa de mortalidad por PAM (%)"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = CCAA == "TOTAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia. Defunciones totales: INE 2024"
  ) %>%
  gtsave("tablas/tab2_3.html")





#3. SOLICITUDES DE LA PAM
## Solicitudes por comunidad autónoma y sexo, con columna de total para ambas variables
tab3_1 <- df %>%
  group_by(CCAA, SEXO) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = SEXO, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Calcular totales nacionales por sexo y total general
nacional <- tab3_1 %>%
  summarise(
    CCAA = "Total",
    H = sum(H, na.rm = TRUE),
    M = sum(M, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE)
  )

# Unir tabla principal con fila de totales nacionales
final_tab3_1 <- bind_rows(tab3_1, nacional) %>%
  gt() %>%
  tab_header(
    title = "Solicitudes de PAM por CCAA y sexo",
    subtitle = "Número de solicitudes"
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

gtsave(final_tab3_1, "tablas/tab3_1.html")

## Solicitudes por enfermedad de base entre 2021 y 2024
# Datos de la tabla
tab3_2 <- tibble::tibble(
  Enfermedad = c("Neurológica", "Oncológica", "Pluripatología", "Respiratoria", "Cardiovascular", "Otras", "No consta"),
  `2021` = c(40, 22, 4, 3, 0, 3, 3),
  `2022` = c(205, 192, 40, 16, 7, 68, 48),
  `2023` = c(266, 271, 49, 24, 13, 105, 38),
  `2024` = c(275, 302, 64, 26, 36, 239, 17)
) %>%
  mutate(Total = `2021` + `2022` + `2023` + `2024`)

# Fila total nacional por año y total general
nacional <- tab3_2 %>%
  summarise(
    Enfermedad = "TOTAL",
    `2021` = sum(`2021`),
    `2022` = sum(`2022`),
    `2023` = sum(`2023`),
    `2024` = sum(`2024`),
    Total = sum(Total)
  )

# Unir tabla principal con fila de totales nacionales y mostrar
final_tab3_2 <- bind_rows(tab3_2, nacional) %>%
  gt() %>%
  tab_header(
    title = "Solicitudes de PAM por enfermedad de base y año",
    subtitle = "2021-2024"
  ) %>%
  cols_label(
    Enfermedad = "Enfermedad",
    `2021` = "2021",
    `2022` = "2022",
    `2023` = "2023",
    `2024` = "2024",
    Total = "Total"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Enfermedad == "TOTAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(final_tab3_2, "tablas/tab3_2.html")

## Solicitudes por tramo de edad y enfermedad de base
tab3_3 <- df %>%
  group_by(TRAMO_EDAD, PATOLOGIA) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = PATOLOGIA, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Calcular totales nacionales por patología y total general
nacional <- tab3_3 %>%
  summarise(
    TRAMO_EDAD = "Total",
    across(where(is.numeric), sum, na.rm = TRUE)
  )

# Unir tabla principal con fila de totales nacionales
final_tab3_3 <- bind_rows(tab3_3, nacional) %>%
  gt() %>%
  tab_header(
    title = "Solicitudes de PAM por tramo de edad y enfermedad de base",
    subtitle = "Número de solicitudes"
  ) %>%
  cols_label(
    TRAMO_EDAD = "Tramo de Edad",
    Total = "Total"
    # Puedes añadir aquí etiquetas personalizadas para cada patología si lo deseas
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

gtsave(final_tab3_3, "tablas/tab3_3.html")
## Solicitudes por informe favorable/desfavorable MR e informe favorable/desfavorable MC (filas 3 a 8 de excel 1)
## Tasa de solicitudes de PAM nacionales y por comunidad autónoma por habitantes (datos de https://www.ine.es/jaxiT3/Tabla.htm?t=56940)
# Vector de habitantes por CCAA (según INE 2024)
habitantes_CCAA <- tibble::tribble(
  ~CCAA, ~habitantes,
  "ANDALUCÍA", 8631862,
  "ARAGÓN", 1351591,
  "ASTURIAS", 1009599,
  "ILLES BALEARS", 1231768,
  "CANARIAS", 2238754,
  "CANTABRIA", 590851,
  "CASTILLA Y LEÓN", 2391682,
  "CASTILLA-LA MANCHA", 2104433,
  "CATALUÑA", 8012231,
  "C. VALENCIANA", 5319285,
  "EXTREMADURA", 1054681,
  "GALICIA", 2705833,
  "MADRID", 7009268,
  "MURCIA", 1568492,
  "NAVARRA", 678333,
  "PAÍS VASCO", 2227684,
  "LA RIOJA", 324184,
  "CEUTA", 83179,
  "MELILLA", 85985
)

# Calcular solicitudes por CCAA (limpieza de nombres)
solicitudes_CCAA <- df %>%
  mutate(
    CCAA = toupper(trimws(CCAA)),
    CCAA = case_when(
      CCAA == "BALEARS, ILLES" ~ "ILLES BALEARS",
      CCAA == "COMUNITAT VALENCIANA" ~ "C. VALENCIANA",
      CCAA == "RIOJA, LA" ~ "LA RIOJA",
      CCAA == "MADRID, COMUNIDAD DE" ~ "MADRID",
      CCAA == "MURCIA, REGIÓN DE" ~ "MURCIA",
      CCAA == "NAVARRA, COMUNIDAD FORAL DE" ~ "NAVARRA",
      CCAA == "PAIS VASCO" ~ "PAÍS VASCO",
      TRUE ~ CCAA
    ),
    CCAA = ifelse(is.na(CCAA) | CCAA == "" | CCAA == "PERDIDOS", "Desconocido", CCAA)
  ) %>%
  group_by(CCAA) %>%
  summarise(solicitudes = n(), .groups = "drop")

# Unir habitantes y solicitudes
tab3_5 <- habitantes_CCAA %>%
  left_join(solicitudes_CCAA, by = "CCAA") %>%
  mutate(
    solicitudes = ifelse(is.na(solicitudes), 0, solicitudes),
    tasa_solicitudes = round((solicitudes / habitantes) * 100000, 2) # por 100.000 habitantes
  )

total_nacional <- tibble(
  CCAA = "TOTAL",
  habitantes = sum(habitantes_CCAA$habitantes),
  solicitudes = sum(tab3_5$solicitudes),
  tasa_solicitudes = round((sum(tab3_5$solicitudes) / sum(habitantes_CCAA$habitantes)) * 100000, 2)
)

final_tab3_5 <- bind_rows(tab3_5, total_nacional) %>%
  gt() %>%
  tab_header(
    title = "Tasa de solicitudes de PAM por 100.000 habitantes",
    subtitle = "Por Comunidad Autónoma y nacional (2024)"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    habitantes = "Habitantes (2024)",
    solicitudes = "Solicitudes",
    tasa_solicitudes = "Tasa por 100.000 habitantes"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = CCAA == "TOTAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: INE 2024 y elaboración propia"
  )

gtsave(final_tab3_5, "tablas/tab3_5.html")


#4. PRESTACIONES DE LA PAM
#4.1. CARACTERÍSTICAS DE LA PERSONA SOLICITANTE

## Prestaciones por C.A y sexo
tab4_1_1 <- df %>%
  mutate(
    Fecha.prestacion = trimws(as.character(Fecha.prestacion)),
    CCAA = trimws(as.character(CCAA)),
    SEXO = toupper(trimws(as.character(SEXO)))
  ) %>%
  filter(!is.na(Fecha.prestacion) & Fecha.prestacion != "") %>%
  mutate(
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
nacional <- tab4_1_1 %>%
  summarise(
    CCAA = "Total",
    H = sum(H, na.rm = TRUE),
    M = sum(M, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE)
  )

# Unir tabla principal con fila de totales nacionales y mostrar
final_tab4_1_1 <- bind_rows(tab4_1_1, nacional) %>%
  gt() %>%
  tab_header(
    title = "Prestaciones por CCAA y sexo",
    subtitle = "Número de prestaciones"
  ) %>%
  cols_label(
    CCAA = "Com. Aut.",
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

gtsave(final_tab4_1_1, "tablas/tab4_1_1.html")


## Prestaciones por tramo de edad
tab4_1_2 <- df %>%
  mutate(
    Fecha.prestacion = trimws(as.character(Fecha.prestacion)),
    TRAMO_EDAD = trimws(as.character(TRAMO_EDAD)),
    EDAD = as.numeric(EDAD) # Asegúrate de que EDAD es numérica
  ) %>%
  filter(!is.na(Fecha.prestacion) & Fecha.prestacion != "") %>%
  group_by(TRAMO_EDAD) %>%
  summarise(
    n = n(),
    media_edad = round(mean(EDAD, na.rm = TRUE), 1),
    mediana_edad = median(EDAD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
nacional_tramo <- tab4_1_2 %>%
  summarise(
    TRAMO_EDAD = "Total",
    n = sum(n, na.rm = TRUE),
    media_edad = round(mean(media_edad, na.rm = TRUE), 1),
    mediana_edad = median(mediana_edad, na.rm = TRUE)
  )

# Unir tabla principal con fila total y mostrar
final_tab4_1_2 <- bind_rows(tab4_1_2, nacional_tramo) %>%
  gt() %>%
  tab_header(
    title = "Número de prestaciones y estadísticos de edad",
    subtitle = "Por Tramo de Edad"
  ) %>%
  cols_label(
    TRAMO_EDAD = "Tramo de Edad",
    n = "Número de prestaciones",
    media_edad = "Edad media",
    mediana_edad = "Edad mediana"
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

gtsave(final_tab4_1_2, "tablas/tab4_1_2.html")

tab4_1_2 <- df %>%
  mutate(
    Fecha.prestacion = trimws(as.character(Fecha.prestacion)),
    TRAMO_EDAD = trimws(as.character(TRAMO_EDAD))
  ) %>%
  filter(!is.na(Fecha.prestacion) & Fecha.prestacion != "") %>%
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
nacional_tramo <- tab4_1_2 %>%
  summarise(
    TRAMO_EDAD = "Total",
    n = sum(n, na.rm = TRUE)
  )

# Unir tabla principal con fila total y mostrar
final_tab4_1_2 <- bind_rows(tab4_1_2, nacional_tramo) %>%
  gt() %>%
  tab_header(
    title = "Número de prestaciones",
    subtitle = "Por Tramo de Edad"
  ) %>%
  cols_label(
    TRAMO_EDAD = "Tramo de Edad",
    n = "Número de prestaciones"
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

gtsave(final_tab4_1_2, "tablas/tab4_1_2.html")
## Prestaciones por enfermedad de base 
tab4_1_3 <- df %>%
  mutate(
    Fecha.prestacion = trimws(as.character(Fecha.prestacion)),
    PATOLOGIA = trimws(as.character(PATOLOGIA))
  ) %>%
  filter(!is.na(Fecha.prestacion) & Fecha.prestacion != "") %>%
  group_by(PATOLOGIA) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))

nacional <- tab4_1_3 %>%
  summarise(
    PATOLOGIA = "Total",
    n = sum(n, na.rm = TRUE)
  )

final_tab4_1_3 <- bind_rows(tab4_1_3, nacional) %>%
  gt() %>%
  tab_header(
    title = "Prestaciones por enfermedad de base",
    subtitle = "Número de prestaciones"
  ) %>%
  cols_label(
    PATOLOGIA = "Enfermedad de base",
    n = "Número de prestaciones"
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

gtsave(final_tab4_1_3, "tablas/tab4_1_3.html")
## Relación prestaciones / solicitudes por comunidades autónomas de forma absoluta y con porcentajes 
tab4_1_4 <- df %>%
  mutate(
    Fecha.prestacion = trimws(as.character(Fecha.prestacion)),
    CCAA = trimws(CCAA),
    CCAA = toupper(CCAA),
    CCAA = ifelse(is.na(CCAA) | CCAA == "" | CCAA == "PERDIDOS", "Desconocido", CCAA)
  ) %>%
  group_by(CCAA) %>%
  summarise(
    total_solicitudes = n(),
    total_prestaciones = sum(!is.na(Fecha.prestacion) & Fecha.prestacion != "")
  ) %>%
  mutate(
    tasa_prestaciones = round((total_prestaciones / total_solicitudes) * 100, 2)
  ) %>%
  ungroup() %>%
  bind_rows(
    tibble(
      CCAA = "Total",
      total_solicitudes = sum(.$total_solicitudes),
      total_prestaciones = sum(.$total_prestaciones),
      tasa_prestaciones = round((sum(.$total_prestaciones) / sum(.$total_solicitudes)) * 100, 2)
    )
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

gtsave(tab4_1_4, "tablas/tab4_1_4.html")


# Tabla de solicitudes por modalidad de inicio y CCAA (datos manuales)
modalidad_inicio <- tibble::tribble(
  ~Modalidad, ~Total, ~ANDALUCÍA, ~ARAGÓN, ~`ASTURIAS, PRINCIPADO DE`, ~`BALEARS, ILLES`, ~CANARIAS, ~CANTABRIA, ~`CASTILLA-LA MANCHA`, ~`CASTILLA Y LEÓN`, ~CATALUÑA, ~`COMUNITAT VALENCIANA`, ~EXTREMADURA, ~GALICIA, ~`MADRID, COMUNIDAD DE`, ~`MURCIA, REGIÓN DE`, ~`NAVARRA, COMUNIDAD FORAL DE`, ~`PAIS VASCO`, ~`RIOJA, LA`, ~`CEUTA, CIUDAD AUTÓNOMA`, ~`MELILLA, CIUDAD AUTÓNOMA`,
  "Nº de solicitudes tramitadas mediante primera solicitud", 622, 31, 9, 4, 7, 24, 4, 8, 15, 330, 15, 4, 15, 67, 3, 17, 64, 5, 0, 0,
  "Nº de solicitudes tramitadas mediante voluntades anticipadas", 54, 0, 1, 0, 2, 2, 1, 0, 0, 28, 1, 0, 0, 7, 0, 1, 10, 2, 0, 0
)

# Transponer para que las CCAA sean filas y las modalidades columnas
modalidad_inicio_long <- modalidad_inicio %>%
  tidyr::pivot_longer(-Modalidad, names_to = "CCAA", values_to = "Solicitudes") %>%
  tidyr::pivot_wider(names_from = Modalidad, values_from = Solicitudes)

# Mover la fila TOTAL al final
modalidad_inicio_long <- modalidad_inicio_long %>%
  mutate(es_total = ifelse(CCAA == "Total", 1, 0)) %>%
  arrange(es_total) %>%
  mutate(CCAA = ifelse(CCAA == "Total", "TOTAL", CCAA)) %>%
  select(-es_total)

# Tabla gt
modalidad_inicio_long %>%
  gt() %>%
  tab_header(
    title = "Solicitudes de PAM por modalidad de inicio y CCAA",
    subtitle = "Número de solicitudes por comunidad autónoma y modalidad"
  ) %>%
  cols_label(
    CCAA = "Comunidad Autónoma",
    `Nº de solicitudes tramitadas mediante primera solicitud` = "Primera solicitud",
    `Nº de solicitudes tramitadas mediante voluntades anticipadas` = "Voluntades anticipadas"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = CCAA == "TOTAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia. Datos manuales."
  ) %>%
  gtsave("tablas/tab3_4.html")
