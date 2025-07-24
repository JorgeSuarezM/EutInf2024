##ESTRUCTURA DEL INFORME DE ESTADÍSTICAS DE LA PAM

#2. ESTADÍSTICAS GENERALES DE LA PAM
## Nº solicitudes por C.A 
tab2_1 <- df %>%
  mutate(
    ccaa = trimws(ccaa),
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "Desconocido", ccaa)
  ) %>%
  group_by(ccaa) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  bind_rows(
    tibble(ccaa = "Total", n = sum(.$n))
  ) %>%
  gt() %>%
  tab_header(title = "Nº de solicitudes de PAM por CCAA") %>%
  tab_spanner(label = "Resultados", columns = c(ccaa, n)) %>%
  cols_label(ccaa = "Comunidad Autónoma", n = "Número de solicitudes") %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = ccaa == "Total"
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
  filter(fallecimiento_tramitacion == 1) %>% 
  mutate(
    ccaa = trimws(ccaa),
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "Desconocido", ccaa)
  ) %>%
  group_by(ccaa) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  bind_rows(
    tibble(ccaa = "Total", n = sum(.$n))
  ) %>%
  gt() %>%
  tab_header( # Añadir título y subtítulo
    title = "Número de fallecidos durante la tramitación de la PAM",
    subtitle = "Por Comunidad Autónoma"
  ) %>%
  cols_label( # Etiquetas de las columnas
    ccaa = "Com. Aut.",
    n = "Número de fallecidos durante la tramitación de la PAM"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = ccaa == "Total"
    )
  ) %>%
  tab_source_note( # Añadir nota de la fuente
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab2_2, "tablas/tab2_2.html")

## Tasas de mortalidad por PAM (entre total fallecidos por C.A. y total nacional)
# 1. Defunciones totales por ccaa (nombres igual que en df, pero limpiados)
defunciones_ccaa <- tibble::tribble(
  ~ccaa, ~total_muertes,
  "Andalucía", 73485,
  "Aragón", 13928,
  "Asturias", 13137,
  "Islas Baleares", 9003,
  "Canarias", 17786,
  "Cantabria", 6399,
  "Castilla y León", 29505,
  "Castilla-La Mancha", 20042,
  "Cataluña", 67505,
  "Comunidad Valenciana", 46350,
  "Extremadura", 11924,
  "Galicia", 32716,
  "Madrid", 49974,
  "Región de Murcia", 11856,
  "Navarra", 6016,
  "País Vasco", 22593,
  "La Rioja", 3249,
  "Ceuta", 541,
  "Melilla", 430
) 

# 2. Agrupa y cuenta prestaciones por PAM por ccaa (usando el nombre correcto de la columna y limpieza)
fallecidos_PAM_ccaa <- df %>%
  group_by(ccaa) %>%
  summarise(total_prestaciones = sum(!is.na(fecha_prestacion))) %>%
  ungroup()

# 3. Une ambos data frames asegurando que todas las ccaa estén presentes
tab2_4 <- defunciones_ccaa %>%
  left_join(fallecidos_PAM_ccaa, by = "ccaa") %>%
  mutate(
    total_prestaciones = ifelse(is.na(total_prestaciones), 0, total_prestaciones),
    tasa_mortalidad = round((total_prestaciones / total_muertes) * 100, 2)
  )

# 4. Añade fila total
tab2_3 <- tab2_4 %>%
  bind_rows(
    tibble(
      ccaa = "TOTAL",
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
    ccaa = "Comunidad Autónoma",
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
      rows = ccaa == "TOTAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia. Defunciones totales: INE 2024"
  ) %>%
  gtsave("tablas/tab2_3.html")





#3. SOLICITUDES DE LA PAM
## Solicitudes por comunidad autónoma y sexo, con columna de total para ambas variables
tab3_1 <- df %>%
  mutate(sexo = trimws(tolower(sexo))) %>%
  filter(!is.na(sexo) & sexo != "") %>%
  group_by(ccaa, sexo) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Calcular totales nacionales por sexo y total general
nacional <- tab3_1 %>%
  summarise(
    ccaa = "Total",
    across(where(is.numeric), ~sum(.x, na.rm = TRUE))
  )

# Unir tabla principal con fila de totales nacionales
tab3_1 <- tab3_1 %>%
  mutate(
    H = if ("h" %in% names(.)) h else 0,
    M = if ("m" %in% names(.)) m else 0
  ) %>%
  select(ccaa, H, M, Total)
nacional <- tab3_1 %>%
  summarise(
    ccaa = "Total",
    H = sum(H, na.rm = TRUE),
    M = sum(M, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE)
  )
final_tab3_1 <- bind_rows(tab3_1, nacional) %>%
  select(ccaa, Total, H, M) %>%
  gt() %>%
  tab_header(
    title = "Solicitudes de PAM por ccaa y sexo",
    subtitle = "Número de solicitudes"
  ) %>%
  cols_label(
    ccaa = "Comunidad Autónoma",
    Total = "Total",
    H = "Hombres",
    M = "Mujeres"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Total)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = ccaa == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(final_tab3_1, "tablas/tab3_1.html")

## Solicitudes por enfermedad de base entre 2021 y 2024
# Datos de la tabla
tab3_2 <- tibble::tibble(
  Enfermedad = c("Neurológica", "Oncológica", "Pluripatología", "Respiratoria", "Cardiovascular", "Reumatología y patología osteomuscular", "Otras", "No consta"),
  `2021` = c(40, 22, 4, 3, 0, NA, 0, 3),
  `2022` = c(205, 192, 40, 16, 7, NA, 0, 68),
  `2023` = c(266, 271, 49, 24, 13, NA, 0, 105),
  `2024` = c(305, 276, 42, 33, 19, 12, 87, 204)
)

# Fila total nacional por año
nacional <- tab3_2 %>%
  summarise(
    Enfermedad = "TOTAL",
    `2021` = sum(`2021`, na.rm = TRUE),
    `2022` = sum(`2022`, na.rm = TRUE),
    `2023` = sum(`2023`, na.rm = TRUE),
    `2024` = sum(`2024`, na.rm = TRUE)
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
    `2024` = "2024"
  ) %>%
  fmt_missing(
    columns = c("2021", "2022", "2023"),
    missing_text = "-"
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



#Solicitudes por tramo de edad y enfermedad de base (tabla 3.3)


# Solicitudes por tramo de edad y enfermedad de base (tabla 3.3) siguiendo la estructura de tab3_1

# Añadir categoría 'No consta' para patología y edad
tab3_3 <- df %>%
  mutate(
    patologia = ifelse(is.na(patologia) | trimws(patologia) == "", "No consta", stringr::str_to_title(trimws(tolower(patologia)))),
    edad = ifelse(is.na(edad) | trimws(edad) == "", "No consta", trimws(tolower(edad)))
  ) %>%
  group_by(edad, patologia) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = patologia, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Calcular totales nacionales por patología y total general
nacional <- tab3_3 %>%
  summarise(
    edad = "Total",
    across(where(is.numeric), ~sum(.x, na.rm = TRUE))
  )

# Unir tabla principal con fila de totales nacionales
final_tab3_3 <- bind_rows(tab3_3, nacional)



# Reordenar columnas: edad, patologías (alfabéticamente, pero 'No consta' al final), Total al final
pat_cols <- setdiff(names(final_tab3_3), c("edad", "Total"))
pat_cols_ord <- c(sort(setdiff(pat_cols, "No consta")), "No consta")
final_tab3_3 <- final_tab3_3 %>%
  select(edad, all_of(pat_cols_ord), Total)

final_tab3_3_gt <- final_tab3_3 %>%
  gt() %>%
  tab_header(
    title = "Solicitudes de PAM por tramo de edad y enfermedad de base",
    subtitle = "Número de solicitudes"
  ) %>%
  cols_label(
    edad = "Tramo de Edad",
    Total = "Total"
    # Puedes añadir aquí etiquetas personalizadas para cada patología si lo deseas
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Total)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = edad == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(final_tab3_3_gt, "tablas/tab3_3.html")
## Solicitudes por informe favorable/desfavorable MR e informe favorable/desfavorable MC (filas 3 a 8 de excel 1)
## Tasa de solicitudes de PAM nacionales y por comunidad autónoma por habitantes (datos de https://www.ine.es/jaxiT3/Tabla.htm?t=56940)
# Vector de habitantes por ccaa (según INE 2024)
habitantes_ccaa <- tibble::tribble(
  ~ccaa, ~habitantes,
  "Andalucía", 8631862,
  "Aragón", 1351591,
  "Asturias", 1009599,
  "Islas Baleares", 1231768,
  "Canarias", 2238754,
  "Cantabria", 590851,
  "Castilla y León", 2391682,
  "Castilla-La Mancha", 2104433,
  "Cataluña", 8012231,
  "Comunidad Valenciana", 5319285,
  "Extremadura", 1054681,
  "Galicia", 2705833,
  "Madrid", 7009268,
  "Región de Murcia", 1568492,
  "Navarra", 678333,
  "País Vasco", 2227684,
  "La Rioja", 324184,
  "Ceuta", 83179,
  "Melilla", 85985
)

# Calcular solicitudes por ccaa (limpieza de nombres)
solicitudes_ccaa <- df %>%
  mutate(ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "Desconocido", ccaa)) %>%
  group_by(ccaa) %>%
  summarise(solicitudes = n(), .groups = "drop")

# Unir habitantes y solicitudes
tab3_5 <- habitantes_ccaa %>%
  left_join(solicitudes_ccaa, by = "ccaa") %>%
  mutate(
    solicitudes = ifelse(is.na(solicitudes), 0, solicitudes),
    tasa_solicitudes = round((solicitudes / habitantes) * 100000, 2) # por 100.000 habitantes
  )

total_nacional <- tibble(
  ccaa = "TOTAL",
  habitantes = sum(habitantes_ccaa$habitantes),
  solicitudes = sum(tab3_5$solicitudes),
  tasa_solicitudes = round((sum(tab3_5$solicitudes) / sum(habitantes_ccaa$habitantes)) * 100000, 2)
)

final_tab3_5 <- bind_rows(tab3_5, total_nacional) %>%
  gt() %>%
  tab_header(
    title = "Tasa de solicitudes de PAM por 100.000 habitantes",
    subtitle = "Por Comunidad Autónoma y nacional (2024)"
  ) %>%
  cols_label(
    ccaa = "Comunidad Autónoma",
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
      rows = ccaa == "TOTAL"
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
    fecha_prestacion = trimws(as.character(fecha_prestacion)),
    ccaa = as.character(trimws(ccaa)),
    sexo = toupper(trimws(as.character(sexo)))
  ) %>%
  filter(!is.na(fecha_prestacion) & fecha_prestacion != "") %>%
  mutate(
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "Desconocido", ccaa),
    sexo = ifelse(sexo %in% c("H", "M"), sexo, NA)
  ) %>%
  filter(!is.na(sexo)) %>%
  group_by(ccaa, sexo) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
  mutate(
    H = if ("H" %in% names(.)) H else 0,
    M = if ("M" %in% names(.)) M else 0,
    Total = H + M
  )


# Calcular totales nacionales por sexo y total general
nacional <- tab4_1_1 %>%
  summarise(
    ccaa = as.character("Total"),
    H = sum(H, na.rm = TRUE),
    M = sum(M, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE)
  )

# Unir tabla principal con fila de totales nacionales y mostrar
final_tab4_1_1 <- bind_rows(tab4_1_1, nacional) %>%
  gt() %>%
  tab_header(
    title = "Prestaciones por ccaa y sexo",
    subtitle = "Número de prestaciones"
  ) %>%
  cols_label(
    ccaa = "Com. Aut.",
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
      rows = ccaa == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(final_tab4_1_1, "tablas/tab4_1_1.html")


## Prestaciones por tramo de edad
tab4_1_2 <- df %>%
  mutate(
    fecha_prestacion = trimws(as.character(fecha_prestacion)),
    edad = ifelse(is.na(edad) | trimws(edad) == "", "No consta", trimws(as.character(edad)))
  ) %>%
  filter(!is.na(fecha_prestacion) & fecha_prestacion != "") %>%
  group_by(edad) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    edad = case_when(
      edad == "1-Tramo de edad (<30)" ~ "Menores de 30 años",
      edad == "2-Tramo de edad (30-39)" ~ "Entre 30 y 39 años",
      edad == "3-Tramo de edad (40-49)" ~ "Entre 40 y 49 años",
      edad == "4-Tramo de edad (50-59)" ~ "Entre 50 y 59 años",
      edad == "4-Tramo de edad (60-69)" ~ "Entre 60 y 69 años",
      edad == "5-Tramo de edad (70-79)" ~ "Entre 70 y 79 años",
      edad == "6-Tramo de edad (> 80)" ~ "Mayores de 80 años",
      TRUE ~ edad
    )
  )

# Calcular fila total
nacional_tramo <- tab4_1_2 %>%
  summarise(
    edad = "Total",
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
    edad = "Tramo de Edad",
    n = "Número de prestaciones"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = edad == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(final_tab4_1_2, "tablas/tab4_1_2.html")
## Prestaciones por enfermedad de base 
tab4_1_5 <- df %>%
  mutate(
    fecha_prestacion = trimws(as.character(fecha_prestacion)),
    patologia = trimws(as.character(patologia)),
    patologia = ifelse(is.na(patologia) | trimws(patologia) == "", "No consta", trimws(as.character(patologia)))
  ) %>%
  filter(!is.na(fecha_prestacion) & fecha_prestacion != "") %>%
  group_by(patologia) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))

nacional <- tab4_1_5 %>%
  summarise(
    patologia = "Total",
    n = sum(n, na.rm = TRUE)
  )

final_tab4_1_5 <- bind_rows(tab4_1_5, nacional) %>%
  gt() %>%
  tab_header(
    title = "Prestaciones por enfermedad de base",
    subtitle = "Número de prestaciones"
  ) %>%
  cols_label(
    patologia = "Enfermedad de base",
    n = "Número de prestaciones"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = patologia == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(final_tab4_1_5, "tablas/tab4_1_5.html")
## Relación prestaciones / solicitudes por comunidades autónomas de forma absoluta y con porcentajes 
tab4_1_4 <- df %>%
  mutate(
    fecha_prestacion = trimws(as.character(fecha_prestacion)),
    ccaa = trimws(ccaa),
    ccaa = toupper(ccaa),
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "Desconocido", ccaa)
  ) %>%
  group_by(ccaa) %>%
  summarise(
    total_solicitudes = n(),
    total_prestaciones = sum(!is.na(fecha_prestacion) & fecha_prestacion != "")
  ) %>%
  mutate(
    tasa_prestaciones = round((total_prestaciones / total_solicitudes) * 100, 2)
  ) %>%
  ungroup() %>%
  bind_rows(
    tibble(
      ccaa = "Total",
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
    ccaa = "Com. Aut.",
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
      rows = ccaa == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia"
  )

gtsave(tab4_1_4, "tablas/tab4_1_4.html")


# Tabla de solicitudes por modalidad de inicio y ccaa (datos manuales)
modalidad_inicio <- tibble::tribble(
  ~Modalidad, ~Total, ~ANDALUCÍA, ~ARAGÓN, ~`ASTURIAS, PRINCIPADO DE`, ~`BALEARS, ILLES`, ~CANARIAS, ~CANTABRIA, ~`CASTILLA-LA MANCHA`, ~`CASTILLA Y LEÓN`, ~CATALUÑA, ~`COMUNITAT VALENCIANA`, ~EXTREMADURA, ~GALICIA, ~`MADRID, COMUNIDAD DE`, ~`MURCIA, REGIÓN DE`, ~`NAVARRA, COMUNIDAD FORAL DE`, ~`PAIS VASCO`, ~`RIOJA, LA`, ~`CEUTA, CIUDAD AUTÓNOMA`, ~`MELILLA, CIUDAD AUTÓNOMA`,
  "Nº de solicitudes tramitadas mediante primera solicitud", 588, 30, 9, 6, 6, 24, 4, 8, 15, 306, 15, 4, 15, 66, 4, 17, 53, 6, 0, 0,
  "Nº de solicitudes tramitadas mediante voluntades anticipadas", 53, 0, 1, 0, 2, 2, 1, 0, 0, 26, 1, 0, 0, 7, 0, 1, 10, 2, 0, 0
)

# Transponer para que las ccaa sean filas y las modalidades columnas
modalidad_inicio_long <- modalidad_inicio %>%
  tidyr::pivot_longer(-Modalidad, names_to = "ccaa", values_to = "Solicitudes") %>%
  tidyr::pivot_wider(names_from = Modalidad, values_from = Solicitudes)

# Mover la fila TOTAL al final
modalidad_inicio_long <- modalidad_inicio_long %>%
  mutate(es_total = ifelse(ccaa == "Total", 1, 0)) %>%
  arrange(es_total) %>%
  mutate(ccaa = ifelse(ccaa == "Total", "TOTAL", ccaa)) %>%
  select(-es_total)

# Tabla gt
modalidad_inicio_long %>%
  gt() %>%
  tab_header(
    title = "Solicitudes de PAM por modalidad de inicio y ccaa",
    subtitle = "Número de solicitudes por comunidad autónoma y modalidad"
  ) %>%
  cols_label(
    ccaa = "Comunidad Autónoma",
    `Nº de solicitudes tramitadas mediante primera solicitud` = "Primera solicitud",
    `Nº de solicitudes tramitadas mediante voluntades anticipadas` = "Voluntades anticipadas"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = ccaa == "TOTAL"
    )
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia. Datos manuales."
  ) %>%
  gtsave("tablas/tab3_4.html")
