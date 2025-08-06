### Tabla 1: Distribución por sexo de solicitantes y beneficiarios
t<-df %>%
mutate(
  Solicitante = 1,
  Beneficiario = if_else(!is.na(fecha_prestacion), 1, 0),
  fallecimiento_tramitacion = as.numeric(fallecimiento_tramitacion),
  denegado = as.numeric(denegado),
  revocacion = as.numeric(revocacion)
)
table(df$Denegado)

tab1<-t %>%
group_by(ccaa) %>%
summarise(
  TotalSolicitantesN = sum(Solicitante),
  TotalSolicitantesP = round((sum(Solicitante) / nrow(t) * 100), digits=2),
  TotalPrestacionesN = sum(Beneficiario),
  TotalPrestacionesP = round((sum(Beneficiario) / nrow(t) * 100), digits=2),
  TotalMuerteDuranteTramitacionN = sum(fallecimiento_tramitacion, na.rm=T),
  TotalMuerteDuranteTramitacionP = round((sum(fallecimiento_tramitacion, na.rm=T) / nrow(t) * 100), digits=2),
  TotalDenegadosN = sum(denegado, na.rm = TRUE),
  TotalDenegadosP = round((sum(denegado, na.rm = TRUE) / nrow(t) * 100), digits=2),
  TotalRevocadosN = sum(revocacion, na.rm = TRUE),
  TotalRevocadosP = round((sum(revocacion, na.rm = TRUE) / nrow(t) * 100), digits=2)
)
tab1b<-t %>%
summarise(
    ccaa = "Total",
    TotalSolicitantesN = sum(Solicitante),
    TotalSolicitantesP = round((sum(Solicitante) / nrow(t) * 100), digits=2),
    TotalPrestacionesN = sum(Beneficiario),
    TotalPrestacionesP = round((sum(Beneficiario) / nrow(t) * 100), digits=2),
    TotalMuerteDuranteTramitacionN = sum(fallecimiento_tramitacion, na.rm=T),
    TotalMuerteDuranteTramitacionP = round((sum(fallecimiento_tramitacion, na.rm=T) / nrow(t) * 100), digits=2),
    TotalDenegadosN = sum(denegado, na.rm = TRUE),
    TotalDenegadosP = round((sum(denegado, na.rm = TRUE) / nrow(t) * 100), digits=2),
    TotalRevocadosN = sum(revocacion, na.rm = TRUE),
    TotalRevocadosP = round((sum(revocacion, na.rm = TRUE) / nrow(t) * 100), digits=2)
)
tab1 <- bind_rows(tab1, tab1b)

tab1 <- tab1 %>%
    mutate(
        order = case_when(
            ccaa == "Total" ~ 2,
            ccaa == "No consta" ~ 1,
            TRUE ~ 0
        )
    ) %>%
    arrange(order, ccaa) %>%
    select(-order)

t1 <- tab1 %>%
    gt() %>%
        tab_header(
                title = "Distribución general por Comunidad Autónoma",
        ) %>%
        cols_label(
                ccaa = "Comunidad Autónoma",
                TotalSolicitantesN = "Núm.",
                TotalSolicitantesP = "%",
                TotalPrestacionesN = "Núm.",
                TotalPrestacionesP = "%",
                TotalMuerteDuranteTramitacionN = "Núm.",
                TotalMuerteDuranteTramitacionP = "%",
                TotalDenegadosN = "Núm.",
                TotalDenegadosP = "%",
                TotalRevocadosN = "Núm.",
                TotalRevocadosP = "%"
        ) %>%
        tab_spanner(
                label = "Solicitudes",
                columns = c(TotalSolicitantesN, TotalSolicitantesP)
        ) %>%
        tab_spanner(
                label = "Prestaciones",
                columns = c(TotalPrestacionesN, TotalPrestacionesP)
        ) %>%
        tab_spanner(
            label = "Muertes durante tramitación",
            columns = c(TotalMuerteDuranteTramitacionN, TotalMuerteDuranteTramitacionP)
        ) %>%
        tab_spanner(
            label = "Denegaciones",
            columns = c(TotalDenegadosN, TotalDenegadosP)
        ) %>%
        tab_spanner(
            label = "Revocaciones",
            columns = c(TotalRevocadosN, TotalRevocadosP)
        ) %>%
        cols_align(
            align = "center",
            columns = -ccaa
        ) %>%
        tab_style(
            style = cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(2)
            ),
            locations = cells_body(
            rows = 19
            )
        ) %>%
        tab_source_note( # Añadir nota de la fuente
            source_note = "Nota: Todos los porcentajes se dan con respecto del total de solicitudes."
            ) %>%
        tab_style(
            style = list(
            cell_fill(color = "gray40"),
            cell_text(color = "white")
            ),
            locations = cells_body(
            rows = 19
            )
        )  %>%
        gt_theme_espn()
gtsave(t1, "script_tablas/tablas_def/tabla_1.html")

### Tabla 2: Tasa de solicitudes de PAM por 100.000 habitantes
habitantes_ccaa <- tibble::tribble(
  ~ccaa, ~habitantes,
  "Andalucía", 8631862,
  "Aragón", 1351591,
  "Asturias", 1009599,
  "Canarias", 2238754,
  "Cantabria", 590851,
  "Castilla y León", 2391682,
  "Castilla-La Mancha", 2104433,
  "Cataluña", 8012231,
  "Comunidad Valenciana", 5319285,
  "Extremadura", 1054681,
  "Galicia", 2705833,
  "Islas Baleares", 1231768,
  "Madrid", 7009268,
  "Murcia", 1568492,
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

# Asegurar que Ceuta tenga 0 si no hay datos
if (!"Ceuta" %in% solicitudes_ccaa$ccaa) {
  solicitudes_ccaa <- bind_rows(solicitudes_ccaa, tibble(ccaa = "Ceuta", solicitudes = 0))
}

# Calcular total nacional de solicitudes
total_solicitudes <- sum(solicitudes_ccaa$solicitudes, na.rm = TRUE)

# Unir habitantes y solicitudes y calcular porcentaje
t2 <- habitantes_ccaa %>%
  left_join(solicitudes_ccaa, by = "ccaa") %>%
  mutate(
    #solicitudes = ifelse(is.na(solicitudes), 0, solicitudes),
    pct_solicitudes = round((solicitudes / total_solicitudes) * 100, 2),
    tasa_solicitudes = round((solicitudes / habitantes) * 100000, 2)
  )

total_nacional <- tibble(
  ccaa = "TOTAL",
  habitantes = sum(habitantes_ccaa$habitantes),
  solicitudes = sum(t2$solicitudes),
  pct_solicitudes = 100,
  tasa_solicitudes = round((sum(t2$solicitudes) / sum(habitantes_ccaa$habitantes)) * 100000, 2)
)

final_t2 <- bind_rows(t2, total_nacional) %>%
  select(-habitantes) %>%
  gt() %>%
  tab_header(
    title = "Tasa de solicitudes de PAM por 100.000 habitantes",
    subtitle = "Por Comunidad Autónoma y nacional (2024)"
  ) %>%
  cols_label(
    ccaa = "Comunidad Autónoma",
    solicitudes = "Num",
    pct_solicitudes = "%",
    tasa_solicitudes = "Tasa por 100.000 habitantes"
  ) %>%
  tab_spanner(
    label = "Solicitudes",
    columns = c(solicitudes, pct_solicitudes)
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

gtsave(final_t2, "script_tablas/tablas_def/tabla_2.html")

### Tabla 3: Número de solicitudes por ccaa (n y %), histórico 2021-2024
# Datos de solicitudes y población
solicitudes_historico <- data.frame(
  Comunidad = c("Andalucía", "Aragón", "Asturias", "Canarias", "Cantabria",
                "Castilla-La Mancha", "Castilla y León", "Cataluña", "Comunidad Valenciana",
                "Extremadura", "Galicia", "Islas Baleares", "Madrid", "Murcia", "Navarra", "País Vasco",
                "La Rioja", "Ceuta", "Melilla"),
  S2021 = c(3, 4, 4, 8, 2, 1, 4, 65, 10, 1, 6, 9, 12, 3, 4, 34, 2, 0, 1),
  S2022 = c(46, 26, 19, 21, 17, 20, 25, 175, 46, 11, 17, 22, 62, 5, 19, 40, 5, 0, 0),
  S2023 = c(43, 22, 33, 62, 19, 28, 27, 219, 56, 2, 41, 37, 89, 2, 24, 58, 4, 0, 0),
  S2024 = c(72, 22, 34, 52, 13, 26, 40, 292, 30, 8, 33, 48, 130, 9, 23, 75, 9, 0, 1)
)

tabla_pob <- data.frame(
  Comunidad = c("Andalucía", "Aragón", "Asturias", "Canarias", "Cantabria",
                "Castilla-La Mancha", "Castilla y León", "Cataluña", "Comunidad Valenciana",
                "Extremadura", "Galicia", "Islas Baleares", "Madrid", "Murcia", "Navarra", "País Vasco",
                "La Rioja", "Ceuta", "Melilla"),
  POB_2021 = c(8484804, 1331938, 1012117, 2178924, 584708,
               2052505, 2385223, 7749896, 5067911,
               1061636, 2698177, 1183415, 6726640, 1518279, 662032, 2212628,
               319444, 84071, 86450),
  POB_2022 = c(8511167, 1328215, 1004960, 2185607, 585450,
               2058278, 2375583, 7761823, 5108116,
               1056808, 2692825, 1187043, 6743254, 1529658, 664514, 2205826,
               319617, 83051, 84932),
  POB_2023 = c(8584147, 1341289, 1006060, 2213016, 588387,
               2084086, 2383703, 7901963, 5216195,
               1054306, 2699424, 1209906, 6871903, 1551692, 672155, 2216302,
               322282, 83052, 85493),
  POB_2024 = c(8631862, 1351591, 1009599, 2238754, 590851,
               2104433, 2391682, 8012231, 5319285,
               1054681, 2705833, 1231768, 7009268, 1568492, 678333, 2227684,
               324184, 83179, 85985)
)

# Pivotar solicitudes a largo y limpiar año
solicitudes_largo <- solicitudes_historico %>%
  pivot_longer(cols = -Comunidad, names_to = "Año", values_to = "N") %>%
  mutate(Año = gsub("S", "", Año))

# Pivotar población a largo y limpiar año
pob_largo <- tabla_pob %>%
  pivot_longer(cols = -Comunidad, names_to = "Año", values_to = "Poblacion") %>%
  mutate(Año = gsub("POB_", "", Año))

# Unir y calcular tasa
solicitudes_largo <- solicitudes_largo %>%
  left_join(pob_largo, by = c("Comunidad", "Año")) %>%
  mutate(
    Tasa = round(N / Poblacion * 100000, 2)
  )

# Calcular total nacional por año
totales <- solicitudes_largo %>%
  group_by(Año) %>%
  summarise(Total_Nacional = sum(N, na.rm = TRUE),
            Total_Poblacion = sum(Poblacion, na.rm = TRUE)) %>%
  mutate(Tasa_Nacional = round(Total_Nacional / Total_Poblacion * 100000, 2))

# Volver a ancho: una fila por comunidad, columnas n y tasa por año
solicitudes_ancho <- solicitudes_largo %>%
  select(Comunidad, Año, N, Tasa) %>%
  pivot_wider(
    names_from = Año,
    values_from = c(N, Tasa),
    names_glue = "S{Año}_{.value}"
  )

# Fila total nacional
fila_total <- solicitudes_ancho[1, ]
fila_total[1, ] <- NA
fila_total$Comunidad <- "Total"
for (col in names(fila_total)) {
  if (grepl("_N$", col)) {
    anio <- sub("S([0-9]+)_N$", "\\1", col)
    fila_total[[col]] <- totales$Total_Nacional[totales$Año == anio]
  } else if (grepl("_Tasa$", col)) {
    anio <- sub("S([0-9]+)_Tasa$", "\\1", col)
    fila_total[[col]] <- totales$Tasa_Nacional[totales$Año == anio]
  }
}
solicitudes_tabla <- bind_rows(solicitudes_ancho, fila_total)

# Ordenar para que Total quede al final
solicitudes_tabla <- solicitudes_tabla %>%
  mutate(order = if_else(Comunidad == "Total", 2L, 0L)) %>%
  arrange(order, Comunidad) %>%
  select(-order)

# Crear tabla gt
t3 <- solicitudes_tabla %>%
  gt() %>%
  tab_header(
    title = "Número de solicitudes por CC. AA y tasas por 100.000 habitantes (histórico 2021-2024)"
  ) %>%
  cols_label(
    Comunidad = "CC. AA.",
    S2021_N = "n", S2021_Tasa = "Tasa por 100.000 hab.",
    S2022_N = "n", S2022_Tasa = "Tasa por 100.000 hab.",
    S2023_N = "n", S2023_Tasa = "Tasa por 100.000 hab.",
    S2024_N = "n", S2024_Tasa = "Tasa por 100.000 hab."
  ) %>%
  tab_spanner(
    label = "2021",
    columns = c(S2021_N, S2021_Tasa)
  ) %>%
  tab_spanner(
    label = "2022",
    columns = c(S2022_N, S2022_Tasa)
  ) %>%
  tab_spanner(
    label = "2023",
    columns = c(S2023_N, S2023_Tasa)
  ) %>%
  tab_spanner(
    label = "2024",
    columns = c(S2024_N, S2024_Tasa)
  ) %>%
  cols_align(
    align = "center",
    columns = -Comunidad
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Comunidad == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia a partir de datos de población y solicitudes."
  )

gtsave(t3, "script_tablas/tablas_def/tabla_3.html")


### Tabla 4: Solicitudes por sexo y edad (n) 2024
solicitudes_edad_sexo <- df %>%
  mutate(
    sexo = ifelse(is.na(sexo) | sexo == "", "No consta", sexo)
  ) %>%
  group_by(edad, sexo) %>%
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
  mutate(order = case_when(
    edad == "<30" ~ 0,
    edad == "30-39" ~ 1,
    edad == "40-49" ~ 2,
    edad == "50-59" ~ 3,
    edad == "60-69" ~ 4,
    edad == "70-79" ~ 5,
    edad == ">80" ~ 6,
    edad == "Total" ~ 7,
    edad == "No consta" ~ 8,
    TRUE ~ 99
  )) %>%
  arrange(order) %>%
  select(-order)

# Renombrar columnas h, H, m, M a Hombre y Mujer si existen
colnames(solicitudes_edad_sexo) <- colnames(solicitudes_edad_sexo) %>%
  gsub('^(h|H)$', 'Hombre', .) %>%
  gsub('^(m|M)$', 'Mujer', .)

# Calcular columna Total por fila (enfermedad)
solicitudes_edad_sexo <- solicitudes_edad_sexo %>%
  mutate(Total = rowSums(select(., -edad)))

# Calcular totales generales
total_hombres <- sum(solicitudes_edad_sexo$Hombre, na.rm = TRUE)
total_mujeres <- sum(solicitudes_edad_sexo$Mujer, na.rm = TRUE)
total_general <- sum(solicitudes_edad_sexo$Total, na.rm = TRUE)

# Calcular % de cada celda respecto al total de su sexo y % total por fila
solicitudes_edad_sexo <- solicitudes_edad_sexo %>%
  mutate(
    pct_Hombre = round(Hombre / total_hombres * 51.69, 2),
    pct_Mujer = round(Mujer / total_mujeres * 48.31, 2),
    pct_Total = round(Total / total_general * 100, 2)
  )

# Añadir fila total nacional y calcular % global de Hombre y Mujer
fila_total <- solicitudes_edad_sexo[1, ]
fila_total[1, ] <- NA
fila_total$edad <- "Total"
for (col in names(fila_total)) {
  if (col %in% c("Hombre", "Mujer", "Total")) {
    fila_total[[col]] <- sum(solicitudes_edad_sexo[[col]], na.rm = TRUE)
  }
}
fila_total$pct_Hombre <- 51.69
fila_total$pct_Mujer <- 48.31
fila_total$pct_Total <- 100
solicitudes_edad_sexo <- bind_rows(solicitudes_edad_sexo, fila_total)

# Tabla gt
t4 <- solicitudes_edad_sexo %>%
  gt() %>%
  tab_header(
    title = "Solicitudes por sexo y edad",
    subtitle = "Número y porcentaje de solicitudes por grupo de edad y totales"
  ) %>%
  cols_label(
    edad = "Edad",
    Hombre = "Num",
    pct_Hombre = "%",
    Mujer = "Num",
    pct_Mujer = "%",
    Total = "Num",
    pct_Total = "%"
  ) %>%
  cols_align(
    align = "center",
    columns = -edad
  ) %>%
  tab_spanner(
    label = "Hombre",
    columns = c(Hombre, pct_Hombre),
    id = "spanner_hombre"
  ) %>%
  tab_spanner(
    label = "Mujer",
    columns = c(Mujer, pct_Mujer),
    id = "spanner_mujer"
  ) %>%
  tab_spanner(
    label = "Total",
    columns = c(Total, pct_Total),
    id = "spanner_total"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = edad == "Total"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Total, pct_Total)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = c(Total, pct_Total)
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t4, "script_tablas/tablas_def/tabla_4.html")

###Tabla 5: Solicitudes de PAM por enfermedad de base (n y % respecto al total del año), histórico 2021-2024.
solicitudes_pato_historico <- data.frame(
  patologia = c("Neurología", "Oncología", "Pluripatología", "Reumatología y patología osteomuscular",
                "Cardiovascular", "Respiratoria", "Otra", "No consta"),
  `2021` = c(40, 22, 4, 0, 0, 3, 3, 101),
  `2022` = c(205, 192, 40, 0, 7, 16, 68, 48),
  `2023` = c(266, 271, 49, 0, 13, 24, 105, 38),
  `2024` = c(300, 269, 42, 12, 19, 29, 84, 162)
)
solicitudes_pato_largo <- solicitudes_pato_historico %>%
  pivot_longer(cols = -patologia, names_to = "Año", values_to = "N")
# Calcular total nacional por año
totales_pato <- solicitudes_pato_largo %>%
  group_by(Año) %>%
  summarise(N = sum(N, na.rm = TRUE)) %>%
  rename(Total_Nacional = N)
# Unir para calcular el %
solicitudes_pato_largo <- solicitudes_pato_largo %>%
  left_join(totales_pato, by = "Año") %>%
  mutate(Porcentaje = round((N / Total_Nacional) * 100, 2))
# Volver a formato ancho: una fila por patología, columnas n y % por año
solicitudes_pato_ancho <- solicitudes_pato_largo %>%
  select(patologia, Año, N, Porcentaje) %>%
  pivot_wider(
    names_from = Año,
    values_from = c(N, Porcentaje),
    names_glue = "{Año}_{.value}"
  )
# Construir fila total nacional con los mismos nombres y orden que solicitudes_pato_ancho
fila_total_pato <- solicitudes_pato_ancho[1, ]
fila_total_pato[1, ] <- NA
fila_total_pato$patologia <- "Total"
for (col in names(fila_total_pato)) {
  if (grepl("_N$", col)) {
    anio <- sub("_N$", "", col)
    fila_total_pato[[col]] <- totales_pato$Total_Nacional[totales_pato$Año == anio]
  } else if (grepl("_Porcentaje$", col)) {
    fila_total_pato[[col]] <- 100
  }
}
solicitudes_pato_tabla <- bind_rows(solicitudes_pato_ancho, fila_total_pato)
# Ordenar para que Total quede al final
solicitudes_pato_tabla <- solicitudes_pato_tabla %>%
  mutate(order = if_else(patologia == "Total", 99L, 0L)) %>%
  arrange(order) %>%
  select(-order)
# Crear tabla
t5 <- solicitudes_pato_tabla %>%
  gt() %>%
  tab_header(
    title = "Solicitudes de PAM por enfermedad de base y año",
    subtitle = "Núm. y % sobre el total nacional"
  ) %>%
  cols_label(
    patologia = "Enfermedad de base",
    X2021_N = "Núm.", X2021_Porcentaje = "%",
    X2022_N = "Núm.", X2022_Porcentaje = "%",
    X2023_N = "Núm.", X2023_Porcentaje = "%",
    X2024_N = "Núm.", X2024_Porcentaje = "%"
  ) %>%
  tab_spanner(
    label = "2021",
    columns = c(X2021_N, X2021_Porcentaje)
  ) %>%
  tab_spanner(
    label = "2022",
    columns = c(X2022_N, X2022_Porcentaje)
  ) %>%
  tab_spanner(
    label = "2023",
    columns = c(X2023_N, X2023_Porcentaje)
  ) %>%
  tab_spanner(
    label = "2024",
    columns = c(X2024_N, X2024_Porcentaje)
  ) %>%
  cols_align(
    align = "center",
    columns = -patologia
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
    source_note = "Nota: Elaboración propia.")

gtsave(t5, "script_tablas/tablas_def/tabla_5.html")

### Tabla 6: Solicitudes por país de nacimiento (n y %), 2024
tabla6 <- df %>%
  mutate(pais_nacimiento = ifelse(is.na(pais_nacimiento) | pais_nacimiento == "", "No consta", pais_nacimiento)) %>%
  group_by(pais_nacimiento) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  arrange(desc(n))

# Añadir fila total
fila_total <- data.frame(pais_nacimiento = "Total",
                        n = sum(tabla6$n),
                        pct = 100)
tabla6 <- bind_rows(tabla6, fila_total)

# Tabla gt
t6 <- tabla6 %>%
  gt() %>%
  tab_header(
    title = "Solicitudes por país de nacimiento, 2024"
  ) %>%
  cols_label(
    pais_nacimiento = "País de nacimiento",
    n = "n",
    pct = "%"
  ) %>%
  tab_spanner(
    label = "Solicitudes",
    columns = c(n, pct)
  ) %>%
  cols_align(
    align = "center",
    columns = c(n, pct)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = pais_nacimiento == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t6, "script_tablas/tablas_def/tabla_6.html")

###Tabla 7: Tasa de prestaciones respecto a solicitudes totales y mortalidad total por CCAA 2024
defunciones_ccaa <- tribble(
  ~ccaa, ~defunciones,
  "Andalucía", 73036,
  "Aragón", 13858,
  "Asturias", 13106,
  "Canarias", 17582,
  "Cantabria", 6103,
  "Castilla y León", 28512,
  "Castilla-La Mancha", 19385,
  "Cataluña", 67298,
  "Comunidad Valenciana", 45826,
  "Extremadura", 11391,
  "Galicia", 32725,
  "Islas Baleares", 9023,
  "Madrid", 48620,
  "Murcia", 11690,
  "Navarra", 6042,
  "País Vasco", 22394,
  "La Rioja", 3306,
  "Ceuta", 525,
  "Melilla", 433
)

# Solicitudes por ccaa
solicitudes_ccaa <- df %>%
  mutate(ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)) %>%
  group_by(ccaa) %>%
  summarise(solicitudes = n(), .groups = "drop")

# Prestaciones por ccaa
prestaciones_ccaa <- df %>%
  filter(!is.na(fecha_prestacion)) %>%
  mutate(ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)) %>%
  group_by(ccaa) %>%
  summarise(prestaciones = n(), .groups = "drop")

# Unir todo
tabla7_raw <- defunciones_ccaa %>%
  left_join(solicitudes_ccaa, by = "ccaa") %>%
  left_join(prestaciones_ccaa, by = "ccaa") %>%
  mutate(
    solicitudes = ifelse(is.na(solicitudes), 0, solicitudes),
    prestaciones = ifelse(is.na(prestaciones), 0, prestaciones),
    tasa_prestacion = round((prestaciones / solicitudes) * 100, 2),
    tasa_mortalidad_pam = round((prestaciones / defunciones) * 100, 2)
  )

tabla7 <- tabla7_raw %>%
  mutate(
    solicitudes = ifelse(ccaa == "Ceuta" & is.na(solicitudes), 0, solicitudes),
    prestaciones = ifelse(ccaa == "Ceuta" & is.na(prestaciones), 0, prestaciones),
    tasa_prestacion = ifelse(ccaa == "Ceuta" & is.na(tasa_prestacion), 0, tasa_prestacion),
    tasa_mortalidad_pam = ifelse(ccaa == "Ceuta" & is.na(tasa_mortalidad_pam), 0, tasa_mortalidad_pam)
  ) %>%
  select(ccaa, solicitudes, prestaciones, tasa_prestacion, tasa_mortalidad_pam)

# Añadir fila total nacional
total_defunciones <- sum(tabla7_raw$defunciones, na.rm = TRUE)
fila_total <- tibble(
  ccaa = "Total",
  solicitudes = sum(tabla7$solicitudes, na.rm = TRUE),
  prestaciones = sum(tabla7$prestaciones, na.rm = TRUE),
  tasa_prestacion = round((sum(tabla7$prestaciones, na.rm = TRUE) / sum(tabla7$solicitudes, na.rm = TRUE)) * 100, 2),
  tasa_mortalidad_pam = round((sum(tabla7$prestaciones, na.rm = TRUE) / total_defunciones) * 100, 2)
)

tabla7 <- bind_rows(tabla7, fila_total)

# Formatear y guardar tabla con gt
t7 <- tabla7 %>%
  gt() %>%
  tab_header(
    title = "Tasa de prestaciones respecto a solicitudes totales y mortalidad total por Comunidad Autónoma, 2024"
  ) %>%
  cols_label(
    ccaa = "Comunidad Autónoma",
    solicitudes = "Solicitudes",
    prestaciones = "Prestaciones",
    tasa_prestacion = "Tasa de prestación (%)",
    tasa_mortalidad_pam = "Tasa de mortalidad por PAM (%)"
  ) %>%
  cols_align(
    align = "center",
    columns = -ccaa
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
    source_note = "Fuente: INE 2024 y elaboración propia"
  )

gtsave(t7, "script_tablas/tablas_def/tabla_7.html")

#### Tabla 8: Tasa de autorización de la CGyE por CCAA 2024
tabla8 <- df %>%
  mutate(
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  group_by(ccaa) %>%
  summarise(
    solicitudes_evaluadas = n(),
    inf_favorables = sum(informe_cgye == "Si", na.rm = TRUE),
    prestaciones_favorables = sum(!is.na(fecha_prestacion) & informe_cgye == "Si", na.rm = TRUE)
  ) %>%
  mutate(
    tasa_inf_favorables = round((inf_favorables / solicitudes_evaluadas) * 100, 2),
    tasa_prestaciones_sobre_favorables = round((prestaciones_favorables / inf_favorables) * 100, 2)
  ) %>%
  select(-prestaciones_favorables) # Elimina la columna

# Añadir fila total nacional
fila_total <- tibble(
  ccaa = "Total",
  solicitudes_evaluadas = sum(tabla8$solicitudes_evaluadas, na.rm = TRUE),
  inf_favorables = sum(tabla8$inf_favorables, na.rm = TRUE),
  tasa_inf_favorables = round((sum(tabla8$inf_favorables, na.rm = TRUE) / sum(tabla8$solicitudes_evaluadas, na.rm = TRUE)) * 100, 2),
  tasa_prestaciones_sobre_favorables = round((sum(df$informe_cgye == "Si" & !is.na(df$fecha_prestacion), na.rm = TRUE) / sum(tabla8$inf_favorables, na.rm = TRUE)) * 100, 2)
)

tabla8 <- bind_rows(tabla8, fila_total)
#
# Tabla gt
t8 <- tabla8 %>%
  gt() %>%
  tab_header(
    title = "Tasa de autorización de la CGyE por CCAA, 2024"
  ) %>%
  cols_label(
    ccaa = "CC. AA.",
    solicitudes_evaluadas = "Solicitudes totales",
    inf_favorables = "Informes favorables",
    tasa_inf_favorables = "Tasa de informes favorables",
    tasa_prestaciones_sobre_favorables = "Tasa de prestaciones sobre informes favorables"
  ) %>%
  cols_align(
    align = "center",
    columns = -ccaa
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
    source_note = "Nota: Elaboración propia."
  )

gtsave(t8, "script_tablas/tablas_def/tabla_8.html")


###Tabla 9: Prestaciones por CCAA, histórico 2021-2024
prestaciones_historico <- data.frame(
  prestaciones_comu = c("Andalucía", "Aragón", "Asturias", "Canarias", "Cantabria",
                  "Castilla-La Mancha", "Castilla y León", "Cataluña", "Comunidad Valenciana",
                  "Extremadura", "Galicia", "Islas Baleares", "Madrid", "Murcia", "Navarra", "País Vasco",
                  "La Rioja", "Ceuta", "Melilla", "Total"),
  P2021 = c(0, 1, 1, 3, 1,
            0, 1, 29, 5,
            0, 2, 6, 6, 2, 2, 15,
            1, 0, 0, 75),
  P2022 = c(27, 91, 8, 10, 6,
            7, 8, 10, 24,
            3, 7, 6, 38, 2, 13, 24,
            4, 0, 0, 288),
  P2023 = c(24, 5, 9, 25, 3,
            9, 12, 94, 26,
            2, 17, 12, 35, 2, 10, 47,
            2, 0, 0, 344),
  P2024 = c(30, 8, 6, 25, 3,
            8, 15, 142, 15,
            4, 16, 17, 59, 4, 14, 50,
            6, 0, 0, 421)
)

prestaciones_clean <- prestaciones_historico %>%
  rename(ccaa = prestaciones_comu) %>%
  mutate(across(starts_with("P"), ~replace_na(., 0)))

# Calcular totales nacionales por año
totales_prestaciones <- prestaciones_clean %>%
  filter(ccaa != "Total") %>%
  summarise(
    total_2021 = sum(P2021, na.rm = TRUE),
    total_2022 = sum(P2022, na.rm = TRUE),
    total_2023 = sum(P2023, na.rm = TRUE),
    total_2024 = sum(P2024, na.rm = TRUE)
  )

# Añadir columnas de porcentaje por año
prestaciones_pct <- prestaciones_clean %>%
  filter(ccaa != "Total") %>%
  mutate(
    pct_2021 = round(P2021 / totales_prestaciones$total_2021 * 100, 2),
    pct_2022 = round(P2022 / totales_prestaciones$total_2022 * 100, 2),
    pct_2023 = round(P2023 / totales_prestaciones$total_2023 * 100, 2),
    pct_2024 = round(P2024 / totales_prestaciones$total_2024 * 100, 2)
  )

# Fila total nacional
fila_total <- tibble(
  ccaa = "Total",
  P2021 = totales_prestaciones$total_2021,
  pct_2021 = 100,
  P2022 = totales_prestaciones$total_2022,
  pct_2022 = 100,
  P2023 = totales_prestaciones$total_2023,
  pct_2023 = 100,
  P2024 = totales_prestaciones$total_2024,
  pct_2024 = 100
)

prestaciones_tabla <- bind_rows(prestaciones_pct, fila_total) %>%
  mutate(order = if_else(ccaa == "Total", 2L, 0L)) %>%
  arrange(order, ccaa) %>%
  select(-order)

# Crear tabla gt
t9 <- prestaciones_tabla %>%
  gt() %>%
  tab_header(
    title = "Prestaciones por Comunidad Autónoma y año",
    subtitle = "Número y porcentaje sobre el total nacional"
  ) %>%
  cols_label(
    ccaa = "Comunidad Autónoma",
    P2021 = "Núm.", pct_2021 = "%",
    P2022 = "Núm.", pct_2022 = "%",
    P2023 = "Núm.", pct_2023 = "%",
    P2024 = "Núm.", pct_2024 = "%"
  ) %>%
  tab_spanner(
    label = "2021",
    columns = c(P2021, pct_2021)
  ) %>%
  tab_spanner(
    label = "2022",
    columns = c(P2022, pct_2022)
  ) %>%
  tab_spanner(
    label = "2023",
    columns = c(P2023, pct_2023)
  ) %>%
  tab_spanner(
    label = "2024",
    columns = c(P2024, pct_2024)
  ) %>%
  cols_align(align = "center", columns = -ccaa) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = ccaa == "Total")
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t9, "script_tablas/tablas_def/tabla_9.html")

### Tabla 10: Prestaciones por edad y sexo (n y %), 2024
prestaciones_edad_sexo <- df %>%
  filter(!is.na(fecha_prestacion)) %>%
  mutate(
    sexo = ifelse(is.na(sexo) | sexo == "", "No consta", sexo)
  ) %>%
  group_by(edad, sexo) %>%
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
  mutate(order = case_when(
    edad == "<30" ~ 0,
    edad == "30-39" ~ 1,
    edad == "40-49" ~ 2,
    edad == "50-59" ~ 3,
    edad == "60-69" ~ 4,
    edad == "70-79" ~ 5,
    edad == ">80" ~ 6,
    edad == "Total" ~ 7,
    edad == "No consta" ~ 8,
    TRUE ~ 99
  )) %>%
  arrange(order) %>%
  select(-order)

# Renombrar columnas h, H, m, M a Hombre y Mujer si existen
colnames(prestaciones_edad_sexo) <- colnames(prestaciones_edad_sexo) %>%
  gsub('^(h|H)$', 'Hombre', .) %>%
  gsub('^(m|M)$', 'Mujer', .)

# Calcular columna Total por fila (edad)
prestaciones_edad_sexo <- prestaciones_edad_sexo %>%
  mutate(Total = rowSums(select(., -edad)))

# Calcular total general
total_general <- sum(prestaciones_edad_sexo$Total, na.rm = TRUE)

# Calcular % de cada celda respecto al total general
prestaciones_edad_sexo <- prestaciones_edad_sexo %>%
  mutate(
    pct_Hombre = round(Hombre / total_general * 100, 2),
    pct_Mujer = round(Mujer / total_general * 100, 2),
    pct_Total = round(Total / total_general * 100, 2)
  )

# Añadir fila total nacional y que los % sean la suma de los % de cada fila
fila_total <- prestaciones_edad_sexo[1, ]
fila_total[1, ] <- NA
fila_total$edad <- "Total"
for (col in names(fila_total)) {
  if (col %in% c("Hombre", "Mujer", "Total")) {
    fila_total[[col]] <- sum(prestaciones_edad_sexo[[col]], na.rm = TRUE)
  }
}
fila_total$pct_Hombre <- sum(prestaciones_edad_sexo$pct_Hombre, na.rm = TRUE)
fila_total$pct_Mujer  <- sum(prestaciones_edad_sexo$pct_Mujer, na.rm = TRUE)
fila_total$pct_Total  <- sum(prestaciones_edad_sexo$pct_Total, na.rm = TRUE)
prestaciones_edad_sexo <- bind_rows(prestaciones_edad_sexo, fila_total)

## Tabla gt
tabla10 <- prestaciones_edad_sexo %>%
  gt() %>%
  tab_header(
    title = "Prestaciones por sexo y edad",
    subtitle = "Número y porcentaje de prestaciones por grupo de edad y totales"
  ) %>%
  cols_label(
    edad = "Edad",
    Hombre = "Num",
    pct_Hombre = "%",
    Mujer = "Num",
    pct_Mujer = "%",
    Total = "Num",
    pct_Total = "%"
  ) %>%
  cols_align(
    align = "center",
    columns = -edad
  ) %>%
  tab_spanner(
    label = "Hombre",
    columns = c(Hombre, pct_Hombre),
    id = "spanner_hombre"
  ) %>%
  tab_spanner(
    label = "Mujer",
    columns = c(Mujer, pct_Mujer),
    id = "spanner_mujer"
  ) %>%
  tab_spanner(
    label = "Total",
    columns = c(Total, pct_Total),
    id = "spanner_total"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = edad == "Total"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Total, pct_Total)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = c(Total, pct_Total)
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(tabla10, "script_tablas/tablas_def/tabla_10.html")

### Tabla 11: Prestaciones por enfermedad de base  (n y %), 2024
# Calcular tabla nacional de prestaciones por enfermedad de base (n y %)
prestaciones_base_nacional <- df %>%
  filter(!is.na(fecha_prestacion)) %>%
  mutate(patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia)) %>%
  count(patologia) %>%
  mutate(
    pct = round(n / sum(n) * 100, 2)
  )

# Ordenar enfermedades (ajusta si es necesario)
orden_enfermedades <- c(
  "Neurológica", "Oncológica", "Pluripatología", "Respiratoria",
  "Cardiovascular", "Reumatología y patología osteomuscular", "Otra", "No consta"
)
prestaciones_base_nacional <- prestaciones_base_nacional %>%
  mutate(patologia = factor(patologia, levels = orden_enfermedades)) %>%
  arrange(patologia)

# Añadir fila Total
total_n <- sum(prestaciones_base_nacional$n)
fila_total <- tibble(
  patologia = "Total",
  n = total_n,
  pct = 100
)
prestaciones_base_nacional <- bind_rows(prestaciones_base_nacional, fila_total)

# Tabla gt con estilos para la tabla nacional
t11 <- prestaciones_base_nacional %>%
  gt() %>%
  tab_header(
    title = "Prestaciones por enfermedad de base"
  ) %>%
  cols_label(
    patologia = "Enfermedad de base",
    n = "Núm.",
    pct = "%"
  ) %>%
  cols_align(
    align = "center",
    columns = c(n, pct)
  ) %>%
  # Fila Total
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = patologia == "Total"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t11, "script_tablas/tablas_def/tabla_11.html")


### Tabla 12: Prestaciones por país de nacimiento (n y %), 2024
tabla12 <- df %>%
  filter(!is.na(fecha_prestacion)) %>%
  mutate(pais_nacimiento = ifelse(is.na(pais_nacimiento) | pais_nacimiento == "", "No consta", pais_nacimiento)) %>%
  group_by(pais_nacimiento) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  arrange(desc(n))

# Añadir fila total
fila_total <- data.frame(pais_nacimiento = "Total",
                        n = sum(tabla18$n),
                        pct = 100)
tabla18 <- bind_rows(tabla18, fila_total)

# Tabla gt
t12 <- tabla12 %>%
  gt() %>%
  tab_header(
    title = "Prestaciones por país de nacimiento, 2024"
  ) %>%
  cols_label(
    pais_nacimiento = "País de nacimiento",
    n = "Num",
    pct = "%"
  ) %>%
  tab_spanner(
    label = "Prestaciones",
    columns = c(n, pct)
  ) %>%
  cols_align(
    align = "center",
    columns = c(n, pct)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = pais_nacimiento == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t12, "script_tablas/tablas_def/tabla_12.html")


### Tabla 13: Especialidad del MR y MC (n y %), 2024
# Calcular n y % para MR
tabla_mr <- df %>%
  mutate(especialidad_mr = ifelse(is.na(especialidad_mr) | especialidad_mr == "", "No consta", especialidad_mr)) %>%
  count(especialidad_mr) %>%
  rename(MR_Num = n)

mr_total <- sum(tabla_mr$MR_Num, na.rm = TRUE)
tabla_mr <- tabla_mr %>%
  mutate(MR_Pct = round(MR_Num / mr_total * 100, 2))

# Calcular n y % para MC
tabla_mc <- df %>%
  mutate(especialidad_mc = ifelse(is.na(especialidad_mc) | especialidad_mc == "", "No consta", especialidad_mc)) %>%
  count(especialidad_mc) %>%
  rename(MC_Num = n)

mc_total <- sum(tabla_mc$MC_Num, na.rm = TRUE)
tabla_mc <- tabla_mc %>%
  mutate(MC_Pct = round(MC_Num / mc_total * 100, 2))

# Unir ambas tablas por especialidad
especialidades <- union(tabla_mr$especialidad_mr, tabla_mc$especialidad_mc)
tabla_13 <- tibble::tibble(Especialidad = especialidades) %>%
  left_join(tabla_mr, by = c("Especialidad" = "especialidad_mr")) %>%
  left_join(tabla_mc, by = c("Especialidad" = "especialidad_mc"))

# Reemplazar NA por 0
tabla_13 <- tabla_13 %>%
  mutate(
    MR_Num = tidyr::replace_na(MR_Num, 0),
    MR_Pct = tidyr::replace_na(MR_Pct, 0),
    MC_Num = tidyr::replace_na(MC_Num, 0),
    MC_Pct = tidyr::replace_na(MC_Pct, 0)
  )

# Añadir fila total
fila_total <- tibble::tibble(
  Especialidad = "Total",
  MR_Num = sum(tabla_13$MR_Num, na.rm = TRUE),
  MR_Pct = 100,
  MC_Num = sum(tabla_13$MC_Num, na.rm = TRUE),
  MC_Pct = 100
)
tabla_13 <- bind_rows(tabla_13, fila_total)

# Ordenar las filas: penúltima 'Otra', última 'No consta', luego 'Total'
orden_especialidades <- c(
  setdiff(tabla_13$Especialidad, c("Otra", "No consta", "Total")),
  "Otra", "No consta", "Total"
)
tabla_13 <- tabla_13 %>%
  mutate(Especialidad = factor(Especialidad, levels = orden_especialidades)) %>%
  arrange(Especialidad)

# Etiquetas para gt
labels_13 <- c(
  Especialidad = "Especialidad",
  MR_Num = "Num",
  MR_Pct = "%",
  MC_Num = "Num",
  MC_Pct = "%"
)

# Crear tabla gt 
t13 <- tabla_13 %>%
  gt() %>%
  tab_header(
    title = "Especialidad del MR y MC",
    subtitle = "Num y %, 2024"
  ) %>%
  cols_label(.list = labels_13) %>%
  cols_align(
    align = "center",
    columns = -Especialidad
  ) %>%
  tab_spanner(
    label = "Médico responsable",
    columns = c("MR_Num", "MR_Pct"),
    id = "spanner_mr"
  ) %>%
  tab_spanner(
    label = "Médico consultor",
    columns = c("MC_Num", "MC_Pct"),
    id = "spanner_mc"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = Especialidad == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t13, "script_tablas/tablas_def/tabla_13.html")


### Tabla 14: Prestaciones por lugar de prestación y CCAA (n y %), 2024

#Calcular tabla base de conteo
tabla14_base <- df %>%
  filter(!is.na(fecha_prestacion)) %>%
  mutate(
    lugar_prestacion = ifelse(is.na(lugar_prestacion) | lugar_prestacion == "", "No consta", lugar_prestacion),
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  count(ccaa, lugar_prestacion) %>%
  tidyr::pivot_wider(
    names_from = lugar_prestacion,
    values_from = n,
    values_fill = 0
  )

#Calcular total por fila (solo para % internos, no se muestra)
tabla14_base <- tabla14_base %>%
  mutate(total_tmp = rowSums(across(-ccaa), na.rm = TRUE))

#Calcular % por lugar de prestación en cada CCAA
lugares <- setdiff(names(tabla14_base), c("ccaa", "total_tmp"))
for (lugar in lugares) {
  tabla14_base[[paste0(lugar, "_pct")]] <- round(100 * tabla14_base[[lugar]] / tabla14_base$total_tmp, 2)
}

#Calcular fila total nacional y añadirla
fila_total <- tabla14_base %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(ccaa = "Total")
for (lugar in lugares) {
  fila_total[[paste0(lugar, "_pct")]] <- round(100 * fila_total[[lugar]] / fila_total$total_tmp, 2)
}
tabla14 <- bind_rows(tabla14_base, fila_total)

#Reordenar columnas: para cada lugar, n y % (sin total)
col_order <- c("ccaa")
for (lugar in lugares) {
  col_order <- c(col_order, lugar, paste0(lugar, "_pct"))
}
tabla14 <- tabla14[, col_order]

#Etiquetas para gt
labels <- list(ccaa = "CC. AA.")
for (lugar in lugares) {
  labels[[lugar]] <- "Núm."
  labels[[paste0(lugar, "_pct")]] <- "%"
}

#Arreglo cutre para añadir Ceuta y Melilla
tot<-tabla14[18,]
tabla14[18,] <- NA
tabla14$ccaa[18] <- "Ceuta"
tabla14[19,]<-NA
tabla14$ccaa[19] <- "Melilla"
tabla14[20,] <- tot
tabla14[18:19, 2:9] <- 0


#Añadir columna de tasa hospital/total
#Detectar el nombre exacto de la columna hospital
nombre_hospital <- grep("hospital", lugares, value = TRUE, ignore.case = TRUE)
if (length(nombre_hospital) == 0) {
  stop("No se encontró columna de hospital en lugares de prestación")
}
#Calcular total de prestaciones por CCAA (sumando solo columnas de lugares)
tabla14$total_prestaciones <- rowSums(tabla14[, lugares], na.rm = TRUE)
#Calcular tasa hospital
tabla14$tasa_hospital <- ifelse(tabla14$total_prestaciones == 0, 0, round(100 * tabla14[[nombre_hospital[1]]] / tabla14$total_prestaciones, 1))

# Insertar la columna después de la última columna de lugares
pos_ultima <- max(match(paste0(lugares, "_pct"), names(tabla14)))
tabla14 <- cbind(
  tabla14[, 1:pos_ultima],
  `Tasa hospital (%)` = tabla14$tasa_hospital,
  tabla14[, (pos_ultima+1):(ncol(tabla14)-2)] # -2 para no repetir las columnas auxiliares
)


# Eliminar la última columna (auxiliar) si no es 'Tasa hospital (%)'
if (names(tabla14)[ncol(tabla14)] != "Tasa hospital (%)") {
  tabla14 <- tabla14[, -ncol(tabla14)]
}
tabla14 <- tabla14[, !names(tabla14) %in% c("total_prestaciones", "tasa_hospital")]

# Añadir etiqueta
labels[["Tasa hospital (%)"]] <- "Tasa hospital (%)"

# Crear la tabla gt con doble fila de encabezado (sin columna total)
t14 <- tabla14 %>%
  gt() %>%
  tab_header(
    title = "Prestaciones por lugar de prestación y CCAA, 2024"
  ) %>%
  cols_label(.list = labels) %>%
  {
    gt_tab <- .
    for (lugar in lugares) {
      gt_tab <- gt_tab %>%
        tab_spanner(
          label = lugar,
          columns = c(lugar, paste0(lugar, "_pct")),
          id = paste0("spanner_", make.names(lugar))
        )
    }
    gt_tab
  } %>%
  cols_align(
    align = "center",
    columns = -ccaa
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
    source_note = "Nota: Elaboración propia."
  )

gtsave(t14, "script_tablas/tablas_def/tabla_14.html")


### Tabla 15: Denegaciones por CC. AA. y tasa de denegación por solicitudes totales 2024
# Calcular solicitudes y denegaciones por CCAA
tabla15_base <- df %>%
  mutate(
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  group_by(ccaa) %>%
  summarise(
    Solicitudes = n(),
    Denegaciones = sum(denegado == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Tasa_denegacion = ifelse(Solicitudes == 0, NA, round(100 * Denegaciones / Solicitudes, 2))
  )

# Añadir fila total nacional
fila_total <- tibble(
  ccaa = "Total",
  Solicitudes = sum(tabla15_base$Solicitudes, na.rm = TRUE),
  Denegaciones = sum(tabla15_base$Denegaciones, na.rm = TRUE),
  Tasa_denegacion = round(100 * sum(tabla15_base$Denegaciones, na.rm = TRUE) / sum(tabla15_base$Solicitudes, na.rm = TRUE), 2)
)

tabla15 <- bind_rows(tabla15_base, fila_total) %>%
  mutate(order = if_else(ccaa == "Total", 2L, 0L)) %>%
  arrange(order, ccaa) %>%
  select(-order)

# Etiquetas para gt
labels_15 <- list(
  ccaa = "CC. AA.",
  Solicitudes = "Solicitudes",
  Denegaciones = "Denegaciones",
  Tasa_denegacion = "Tasa de denegación (%)"
)

# Crear tabla gt
t15 <- tabla15 %>%
  gt() %>%
  tab_header(
    title = "Denegaciones por CC. AA. y tasa de denegación por solicitudes totales 2024"
  ) %>%
  cols_label(.list = labels_15) %>%
  cols_align(
    align = "center",
    columns = -ccaa
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
    source_note = "Nota: Elaboración propia."
  )

gtsave(t15, "script_tablas/tablas_def/tabla_15.html")


### Tabla 16: Denegaciones por tramo de edad y sexo (n, solicitudes y tasa) 2024
# Calcular solicitudes y denegaciones por edad y sexo
tabla_edad_solicitudes <- df %>%
  mutate(
    sexo = case_when(
      sexo %in% c("h", "H", "Hombre") ~ "Hombre",
      sexo %in% c("m", "M", "Mujer") ~ "Mujer",
      TRUE ~ "No consta"
    ),
    order = case_when(
      edad == "<30" ~ 1,
      edad == "30-39" ~ 2,
      edad == "40-49" ~ 3,
      edad == "50-59" ~ 4,
      edad == "60-69" ~ 5,
      edad == "70-79" ~ 6,
      edad == ">80" ~ 7,
      edad == "Total" ~ 100,
      TRUE ~ 99
    )
  ) %>%
  group_by(edad, sexo, order) %>%
  summarise(
    Solicitudes = n(),
    Denegaciones = sum(denegado == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Tasa_denegacion = ifelse(Solicitudes == 0, NA, round(100 * Denegaciones / Solicitudes, 2))
  )

# Pivotear para formato ancho
tabla_edad_wide <- tabla_edad_solicitudes %>%
  select(edad, order, sexo, Solicitudes, Denegaciones, Tasa_denegacion) %>%
  tidyr::pivot_wider(
    names_from = sexo,
    values_from = c(Solicitudes, Denegaciones, Tasa_denegacion),
    values_fill = 0
  )

# Calcular tasa de denegación total por edad (suma de denegaciones / suma de solicitudes por fila)
tabla_edad_wide <- tabla_edad_wide %>%
  mutate(
    Solicitudes_Total = rowSums(select(., starts_with("Solicitudes_")), na.rm = TRUE),
    Denegaciones_Total = rowSums(select(., starts_with("Denegaciones_")), na.rm = TRUE),
    Tasa_denegacion_Total = ifelse(Solicitudes_Total == 0, NA, round(100 * Denegaciones_Total / Solicitudes_Total, 2))
  )

# Añadir fila total
fila_total <- tabla_edad_wide %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(edad = "Total", order = 100)
# Calcular tasa para la fila total
for (sx in c("Hombre", "Mujer", "No consta", "Total")) {
  den_col <- paste0("Denegaciones_", sx)
  sol_col <- paste0("Solicitudes_", sx)
  tasa_col <- paste0("Tasa_denegacion_", sx)
  if (den_col %in% names(fila_total) && sol_col %in% names(fila_total)) {
    fila_total[[tasa_col]] <- ifelse(fila_total[[sol_col]] == 0, NA, round(100 * fila_total[[den_col]] / fila_total[[sol_col]], 2))
  }
}
tabla_edad_wide <- bind_rows(tabla_edad_wide, fila_total)
tabla_edad_wide <- tabla_edad_wide %>% arrange(order)

# Seleccionar y renombrar columnas para gt
cols_gt <- c("edad",
             "Solicitudes_Hombre", "Denegaciones_Hombre", "Tasa_denegacion_Hombre",
             "Solicitudes_Mujer", "Denegaciones_Mujer", "Tasa_denegacion_Mujer",
             "Solicitudes_Total", "Denegaciones_Total", "Tasa_denegacion_Total")
if ("Denegaciones_No consta" %in% names(tabla_edad_wide)) {
  cols_gt <- c(cols_gt, "Solicitudes_No consta", "Denegaciones_No consta", "Tasa_denegacion_No consta")
}
tabla_edad_gt <- tabla_edad_wide[, cols_gt]

# Formatear y guardar tabla con gt
tabla_16 <- tabla_edad_gt %>%
  gt() %>%
  tab_header(
    title = "Denegaciones por tramo de edad y sexo",
    subtitle = "Núm, solicitudes y tasa de denegación (%) por grupo de edad y sexo"
  ) %>%
  cols_label(
    edad = "Edad",
    Solicitudes_Hombre = "Solicitudes",
    Denegaciones_Hombre = "Núm",
    Tasa_denegacion_Hombre = "Tasa de denegación (%)",
    Solicitudes_Mujer = "Solicitudes",
    Denegaciones_Mujer = "Núm",
    Tasa_denegacion_Mujer = "Tasa de denegación (%)",
    Solicitudes_Total = "Solicitudes",
    Denegaciones_Total = "Núm",
    Tasa_denegacion_Total = "Tasa de denegación (%)"
    # Si hay No consta, se añade abajo
  ) %>%
  tab_spanner(
    label = "Hombre",
    columns = c("Solicitudes_Hombre", "Denegaciones_Hombre", "Tasa_denegacion_Hombre"),
    id = "spanner_hombre"
  ) %>%
  tab_spanner(
    label = "Mujer",
    columns = c("Solicitudes_Mujer", "Denegaciones_Mujer", "Tasa_denegacion_Mujer"),
    id = "spanner_mujer"
  ) %>%
  tab_spanner(
    label = "Total",
    columns = c("Solicitudes_Total", "Denegaciones_Total", "Tasa_denegacion_Total"),
    id = "spanner_total"
  ) %>%
  {
    gt_tab <- .
    if ("Denegaciones_No consta" %in% names(tabla_edad_gt)) {
      gt_tab <- gt_tab %>%
        cols_label(
          Solicitudes_No_consta = "Solicitudes",
          Denegaciones_No_consta = "n",
          Tasa_denegacion_No_consta = "Tasa de denegación (%)"
        ) %>%
        tab_spanner(
          label = "No consta",
          columns = c("Solicitudes_No consta", "Denegaciones_No consta", "Tasa_denegacion_No consta"),
          id = "spanner_noconsta"
        )
    }
    gt_tab
  } %>%
  cols_align(
    align = "center",
    columns = -edad
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
    source_note = "Nota: Elaboración propia."
  )

gtsave(tabla_16, "script_tablas/tablas_def/tabla_16.html")


### Tabla 17: Denegaciones por enfermedad de base (n, solicitudes y tasa) 2024
tabla17_base <- df %>%
  mutate(
    patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia)
  ) %>%
  group_by(patologia) %>%
  summarise(
    Solicitudes = n(),
    Denegaciones = sum(denegado == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Tasa_denegacion = ifelse(Solicitudes == 0, NA, round(100 * Denegaciones / Solicitudes, 2))
  )

# Añadir fila total nacional
fila_total <- tibble(
  patologia = "Total",
  Solicitudes = sum(tabla17_base$Solicitudes, na.rm = TRUE),
  Denegaciones = sum(tabla17_base$Denegaciones, na.rm = TRUE),
  Tasa_denegacion = round(100 * sum(tabla17_base$Denegaciones, na.rm = TRUE) / sum(tabla17_base$Solicitudes, na.rm = TRUE), 2)
)

tabla17 <- bind_rows(tabla17_base, fila_total) %>%
  mutate(order = if_else(patologia == "Total", 2L, 0L)) %>%
  arrange(order, patologia) %>%
  select(-order)

# Etiquetas para gt
labels_17 <- list(
  patologia = "Enfermedad de base",
  Denegaciones = "Núm",
  Solicitudes = "Solicitudes",
  Tasa_denegacion = "Tasa de denegación (%)"
)

# Crear tabla gt
t17 <- tabla17 %>%
  gt() %>%
  tab_header(
    title = "Denegaciones por enfermedad de base",
    subtitle = "Núm, solicitudes y tasa de denegación (%) por enfermedad de base"
  ) %>%
  cols_label(.list = labels_17) %>%
  cols_align(
    align = "center",
    columns = -patologia
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
    source_note = "Nota: Elaboración propia."
  )

gtsave(t17, "script_tablas/tablas_def/tabla_17.html")


### Tabla 18: Tasa de denegación por instancia (MR, MC, CGyE) 2024
# Cada denegación solo cuenta en la primera instancia donde aparece 'No'
denegacion_instancia[df$informe_mr == "No" & !is.na(df$informe_mr)] <- "MR"
denegacion_instancia[is.na(denegacion_instancia) & df$informe_mc == "No" & !is.na(df$informe_mc)] <- "MC"
denegacion_instancia[is.na(denegacion_instancia) & df$informe_cgye == "No" & !is.na(df$informe_cgye)] <- "CGyE"

# Asignar la instancia de denegación solo si no hay 'No' en instancias previas
denegacion_instancia <- rep(NA_character_, nrow(df))
denegacion_instancia[df$informe_mr == "No" & !is.na(df$informe_mr)] <- "MR"
denegacion_instancia[
  is.na(denegacion_instancia) & 
  df$informe_mc == "No" & !is.na(df$informe_mc) & 
  !(df$informe_mr == "No" & !is.na(df$informe_mr))
] <- "MC"
denegacion_instancia[
  is.na(denegacion_instancia) & 
  df$informe_cgye == "No" & !is.na(df$informe_cgye) & 
  !(df$informe_mr == "No" & !is.na(df$informe_mr)) & 
  !(df$informe_mc == "No" & !is.na(df$informe_mc))
] <- "CGyE"

tabla18_base <- tibble::tibble(
  Instancia = c("MR", "MC", "CGyE"),
  Solicitudes_que_reciben = c(
    sum(df$informe_mr != "" & !is.na(df$informe_mr)),
    sum(df$informe_mc != "" & !is.na(df$informe_mc)),
    sum(df$informe_cgye != "" & !is.na(df$informe_cgye))
  ),
  Denegaciones = c(
    sum(denegacion_instancia == "MR", na.rm = TRUE),
    sum(denegacion_instancia == "MC", na.rm = TRUE),
    sum(denegacion_instancia == "CGyE", na.rm = TRUE)
  )
)
tabla18_base <- tabla18_base %>%
  mutate(Tasa = ifelse(Solicitudes_que_reciben == 0, NA, round(100 * Denegaciones / Solicitudes_que_reciben, 2)))

# Añadir fila total
fila_total <- tibble::tibble(
  Instancia = "Total",
  Solicitudes_que_reciben = sum(tabla18_base$Solicitudes_que_reciben, na.rm = TRUE),
  Denegaciones = sum(tabla18_base$Denegaciones, na.rm = TRUE),
  Tasa = round(100 * sum(tabla18_base$Denegaciones, na.rm = TRUE) / sum(tabla18_base$Solicitudes_que_reciben, na.rm = TRUE), 2)
)
tabla18 <- bind_rows(tabla18_base, fila_total)

# Etiquetas para gt
labels_18 <- list(
  Instancia = "",
  Solicitudes_que_reciben = "Solicitudes que reciben",
  Denegaciones = "Denegaciones",
  Tasa = "Tasa de denegación (%)"
)

# Crear tabla gt con formato igual a tabla 17
t18 <- tabla18 %>%
  gt() %>%
  tab_header(
    title = "Tasa de denegación por instancia 2024"
  ) %>%
  cols_label(.list = labels_18) %>%
  cols_align(
    align = "center",
    columns = -Instancia
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Instancia == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t18, "script_tablas/tablas_def/tabla_18.html")

###Tabla 19: Reclamaciones a la CGyE por CCAA (n y %), 2024

tabla19 <- df %>%
  mutate(
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  group_by(ccaa) %>%
  summarise(
    reclamaciones_totales = sum(resolucion_fav_cgye != "", na.rm = TRUE),
    favorables = sum(resolucion_fav_cgye == "Si", na.rm = TRUE)
  ) %>%
  mutate(
    pct_favorables = ifelse(reclamaciones_totales == 0, NA, round(100 * favorables / reclamaciones_totales, 2))
  )

# Añadir fila total nacional
fila_total <- tibble(
  ccaa = "Total",
  reclamaciones_totales = sum(tabla19$reclamaciones_totales, na.rm = TRUE),
  favorables = sum(tabla19$favorables, na.rm = TRUE),
  pct_favorables = ifelse(sum(tabla19$reclamaciones_totales, na.rm = TRUE) == 0, NA,
                          round(100 * sum(tabla19$favorables, na.rm = TRUE) / sum(tabla19$reclamaciones_totales, na.rm = TRUE), 2))
)

tabla19 <- bind_rows(tabla19, fila_total)

# Reemplazar NA por 0
tabla19[is.na(tabla19)] <- 0

# Formato gt con estilo en la fila Total
t19 <- tabla19 %>%
  gt() %>%
  tab_header(
    title = "Reclamaciones a la CGyE por CC.AA. 2024"
  ) %>%
  cols_label(
    ccaa = "CC. AA.",
    reclamaciones_totales = "Totales",
    favorables = "Num",
    pct_favorables = "Tasa de reclamaciones aprobadas (%)"
  ) %>%
  tab_spanner(
    label = "Favorables",
    columns = c("favorables", "pct_favorables")
  ) %>%
  cols_align(
    align = "center",
    columns = -ccaa
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
    source_note = "Nota: Elaboración propia."
  )

gtsave(t19, "script_tablas/tablas_def/tabla_19.html")


###Tabla 20: Revocaciones por CC.AA (n y % respecto a solicitudes), 2024
tabla_20 <- df %>%
  group_by(ccaa) %>%
  summarise(
    Solicitudes = n(),
    Revocaciones = sum(revocacion == 1, na.rm = TRUE)
  ) %>%
  mutate(
    Porcentaje = round(Revocaciones / Solicitudes * 100, 2)
  )

# Añadir fila total nacional
fila_total <- tabla_20 %>%
  summarise(
    ccaa = "Total",
    Solicitudes = sum(Solicitudes, na.rm = TRUE),
    Revocaciones = sum(Revocaciones, na.rm = TRUE),
    Porcentaje = round(Revocaciones / Solicitudes * 100, 2)
  )
tabla_20 <- bind_rows(tabla_20, fila_total)

# Formato gt
t20 <- tabla_20 %>%
  gt() %>%
  tab_header(
    title = "Revocaciones por CC.AA.",
    subtitle = "Número y porcentaje respecto a solicitudes, 2024"
  ) %>%
  cols_label(
    ccaa = "CC. AA.",
    Solicitudes = "Solicitudes",
    Revocaciones = "Num",
    Porcentaje = "%"
  ) %>%
  cols_align(
    align = "center",
    columns = -ccaa
  ) %>%
  tab_spanner(
    label = "Revocaciones",
    columns = c("Revocaciones", "Porcentaje"),
    id = "spanner_revocaciones"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = ccaa == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t20, "script_tablas/tablas_def/tabla_20.html")

###Tabla 21: Tasa de revocación frente a solicitudes totales (n y %), histórico 2021-2024
tabla_21 <- tibble::tibble(
  Año = c("2021", "2022", "2023", "2024"),
  Revocaciones_totales = c(7, 1, 21, 48),
  Solicitudes_totales = c(173, 576, 766, 917)
)

# Calcular tasa de revocación por año
tabla_21 <- tabla_21 %>%
  mutate(
    Tasa_revocacion = round(Revocaciones_totales / Solicitudes_totales * 100, 2)
  )

# Añadir fila total
fila_total <- tibble::tibble(
  Año = "Total",
  Revocaciones_totales = sum(tabla_21$Revocaciones_totales, na.rm = TRUE),
  Solicitudes_totales = sum(tabla_21$Solicitudes_totales, na.rm = TRUE),
  Tasa_revocacion = round(sum(tabla_21$Revocaciones_totales, na.rm = TRUE) / sum(tabla_21$Solicitudes_totales, na.rm = TRUE) * 100, 2)
)
tabla_21 <- dplyr::bind_rows(tabla_21, fila_total)

# Etiquetas para gt
labels_21 <- c(
  Año = "Año",
  Revocaciones_totales = "Revocaciones totales",
  Solicitudes_totales = "Solicitudes totales",
  Tasa_revocacion = "Tasa de revocación (%)"
)

# Crear tabla gt 
t21 <- tabla_21 %>%
  gt() %>%
  tab_header(
    title = "Tasa de revocación frente a solicitudes totales",
    subtitle = "Num y %, histórico 2021-2024"
  ) %>%
  cols_label(.list = labels_21) %>%
  cols_align(
    align = "center",
    columns = -Año
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = Año == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t21, "script_tablas/tablas_def/tabla_21.html")

###Tabla 22: Revocaciones por sexo y edad (n y %), 2024
revocaciones_edad_sexo <- df %>%
  filter(revocacion == 1) %>%
  mutate(
    sexo = ifelse(is.na(sexo) | sexo == "", "No consta", sexo)
  ) %>%
  group_by(edad, sexo) %>%
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
  mutate(order = case_when(
    edad == "<30" ~ 0,
    edad == "30-39" ~ 1,
    edad == "40-49" ~ 2,
    edad == "50-59" ~ 3,
    edad == "60-69" ~ 4,
    edad == "70-79" ~ 5,
    edad == ">80" ~ 6,
    edad == "Total" ~ 7,
    edad == "No consta" ~ 8,
    TRUE ~ 99
  )) %>%
  arrange(order) %>%
  select(-order)

# Renombrar columnas h, H, m, M a Hombre y Mujer si existen
colnames(revocaciones_edad_sexo) <- colnames(revocaciones_edad_sexo) %>%
  gsub('^(h|H)$', 'Hombre', .) %>%
  gsub('^(m|M)$', 'Mujer', .)

# Calcular columna Total por fila (edad)
revocaciones_edad_sexo <- revocaciones_edad_sexo %>%
  mutate(Total = rowSums(select(., -edad)))

# Calcular % por sexo y total
total_hombres <- sum(revocaciones_edad_sexo$Hombre, na.rm = TRUE)
total_mujeres <- sum(revocaciones_edad_sexo$Mujer, na.rm = TRUE)
total_revocaciones <- total_hombres + total_mujeres

revocaciones_edad_sexo <- revocaciones_edad_sexo %>%
  mutate(
    pct_Hombre = round(Hombre / total_revocaciones * 100, 2),
    pct_Mujer = round(Mujer / total_revocaciones * 100, 2),
    pct_Total = round(Total / total_revocaciones * 100, 2)
  )

# Añadir fila total nacional
fila_total <- revocaciones_edad_sexo[1, ]
fila_total[1, ] <- NA
fila_total$edad <- "Total"
for (col in names(fila_total)) {
  if (col %in% c("Hombre", "Mujer", "Total")) {
    fila_total[[col]] <- sum(revocaciones_edad_sexo[[col]], na.rm = TRUE)
  }
}
# Los porcentajes de la fila total son la suma de los porcentajes de cada columna
fila_total$pct_Hombre <- round(sum(revocaciones_edad_sexo$pct_Hombre, na.rm = TRUE), 2)
fila_total$pct_Mujer  <- round(sum(revocaciones_edad_sexo$pct_Mujer, na.rm = TRUE), 2)
fila_total$pct_Total  <- round(sum(revocaciones_edad_sexo$pct_Total, na.rm = TRUE), 2)
revocaciones_edad_sexo <- bind_rows(revocaciones_edad_sexo, fila_total)

# Seleccionar y renombrar columnas para gt
cols_gt <- c("edad",
             "Hombre", "pct_Hombre",
             "Mujer", "pct_Mujer",
             "Total", "pct_Total")
revocaciones_edad_gt <- revocaciones_edad_sexo[, cols_gt]

# Formatear y guardar tabla con gt
t22 <- revocaciones_edad_gt %>%
  gt() %>%
  tab_header(
    title = "Revocaciones por sexo y edad",
    subtitle = "Número y porcentaje por grupo de edad y totales"
  ) %>%
  cols_label(
    edad = "Edad",
    Hombre = "Num",
    pct_Hombre = "%",
    Mujer = "Num",
    pct_Mujer = "%",
    Total = "Num",
    pct_Total = "%"
  ) %>%
  cols_align(
    align = "center",
    columns = -edad
  ) %>%
  tab_spanner(
    label = "Hombre",
    columns = c("Hombre", "pct_Hombre"),
    id = "spanner_hombre"
  ) %>%
  tab_spanner(
    label = "Mujer",
    columns = c("Mujer", "pct_Mujer"),
    id = "spanner_mujer"
  ) %>%
  tab_spanner(
    label = "Total",
    columns = c("Total", "pct_Total"),
    id = "spanner_total"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = edad == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t22, "script_tablas/tablas_def/tabla_22.html")

### Tabla 23: Revocaciones por enfermedad de base (n, solicitudes y tasa) 2024
tabla23_base <- df %>%
  mutate(
    patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia)
  ) %>%
  group_by(patologia) %>%
  summarise(
    Solicitudes = n(),
    Revocaciones = sum(revocacion == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Tasa_revocacion = ifelse(Solicitudes == 0, NA, round(100 * Revocaciones / Solicitudes, 2))
  )

# Añadir fila total nacional
fila_total <- tibble(
  patologia = "Total",
  Solicitudes = sum(tabla23_base$Solicitudes, na.rm = TRUE),
  Revocaciones = sum(tabla23_base$Revocaciones, na.rm = TRUE),
  Tasa_revocacion = round(100 * sum(tabla23_base$Revocaciones, na.rm = TRUE) / sum(tabla23_base$Solicitudes, na.rm = TRUE), 2)
)

tabla23 <- bind_rows(tabla23_base, fila_total) %>%
  mutate(order = if_else(patologia == "Total", 2L, 0L)) %>%
  arrange(order, patologia) %>%
  select(-order)

# Etiquetas para gt
labels_23 <- list(
  patologia = "Enfermedad de base",
  Revocaciones = "Núm",
  Solicitudes = "Solicitudes",
  Tasa_revocacion = "Tasa de revocación (%)"
)

# Crear tabla gt
t23 <- tabla23 %>%
  gt() %>%
  tab_header(
    title = "Revocaciones por enfermedad de base",
    subtitle = "Núm, solicitudes y tasa de revocación (%) por enfermedad de base"
  ) %>%
  cols_label(.list = labels_23) %>%
  cols_align(
    align = "center",
    columns = -patologia
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
    source_note = "Nota: Elaboración propia."
  )

gtsave(t23, "script_tablas/tablas_def/tabla_23.html")

###Tabla 24: Fase del proceso en que se revoca (n y % respecto a revocaciones totales) 2024
tabla_24 <- df %>%
  filter(revocacion == 1) %>%
  mutate(
    tipo_revocacion = ifelse(is.na(tipo_revocacion) | tipo_revocacion == "", "No consta", tipo_revocacion)
  ) %>%
  count(tipo_revocacion) %>%
  rename(Num = n)

# Calcular total de revocaciones
total_revocaciones <- sum(tabla_24$Num, na.rm = TRUE)

# Calcular porcentaje por fase
tabla_24 <- tabla_24 %>%
  mutate(
    Porcentaje = round(Num / total_revocaciones * 100, 2)
  )

# Añadir fila total
fila_total <- tibble(
  tipo_revocacion = "Revocaciones totales",
  Num = total_revocaciones,
  Porcentaje = round(sum(tabla_24$Porcentaje, na.rm = TRUE), 2)
)
tabla_24 <- bind_rows(tabla_24, fila_total)

# Etiquetas para gt
labels_24 <- c(
  tipo_revocacion = "Fase de la revocación",
  Num = "Num",
  Porcentaje = "Tasa de revocación por fase (%)"
)

# Crear tabla gt con doble encabezado 
t24 <- tabla_24 %>%
  gt() %>%
  tab_header(
    title = "Fase del proceso en que se revoca",
    subtitle = "Número y % respecto a revocaciones totales, 2024"
  ) %>%
  cols_label(.list = labels_24) %>%
  cols_align(
    align = "center",
    columns = -tipo_revocacion
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = tipo_revocacion == "Revocaciones totales"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t24, "script_tablas/tablas_def/tabla_24.html")

###Tabla 25: Tasa de fallecimientos durante tramitación por CC. AA. (%) 2024
# Calcular solicitudes y fallecimientos por ccaa
solicitudes_fallecimientos <- df %>%
  mutate(
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  group_by(ccaa) %>%
  summarise(
    solicitudes = n(),
    fallecimientos_tramitacion = sum(fallecimiento_tramitacion == "1", na.rm = TRUE)
  ) %>%
  mutate(
    tasa_fallecimientos = round((fallecimientos_tramitacion / solicitudes) * 100, 2)
  )

# Añadir fila total nacional
fila_total <- tibble(
  ccaa = "Total",
  solicitudes = sum(solicitudes_fallecimientos$solicitudes, na.rm = TRUE),
  fallecimientos_tramitacion = sum(solicitudes_fallecimientos$fallecimientos_tramitacion, na.rm = TRUE),
  tasa_fallecimientos = round((sum(solicitudes_fallecimientos$fallecimientos_tramitacion, na.rm = TRUE) / sum(solicitudes_fallecimientos$solicitudes, na.rm = TRUE)) * 100, 2)
)

solicitudes_fallecimientos <- bind_rows(solicitudes_fallecimientos, fila_total)

# Tabla gt
tabla25 <- solicitudes_fallecimientos %>%
  gt() %>%
  tab_header(
    title = "Tasa de fallecimientos durante tramitación por CC. AA. (%) 2024"
  ) %>%
  cols_label(
    ccaa = "CC. AA.",
    solicitudes = "Solicitudes",
    fallecimientos_tramitacion = "Fallecimientos durante tramitación",
    tasa_fallecimientos = "Tasa de fallecimientos"
  ) %>%
  cols_align(
    align = "center",
    columns = -ccaa
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
    source_note = "Nota: Elaboración propia."
  )

gtsave(tabla25, "script_tablas/tablas_def/tabla_25.html")

### Tabla 26: Fallecimientos durante tramitacion por sexo y edad (n y %)
fallecimientos_edad_sexo <- df %>%
  filter(fallecimiento_tramitacion == 1) %>%
  mutate(
    sexo = ifelse(is.na(sexo) | sexo == "", "No consta", sexo)
  ) %>%
  group_by(edad, sexo) %>%
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
  mutate(order = case_when(
    edad == "<30" ~ 0,
    edad == "30-39" ~ 1,
    edad == "40-49" ~ 2,
    edad == "50-59" ~ 3,
    edad == "60-69" ~ 4,
    edad == "70-79" ~ 5,
    edad == ">80" ~ 6,
    edad == "Total" ~ 7,
    edad == "No consta" ~ 8,
    TRUE ~ 99
  )) %>%
  arrange(order) %>%
  select(-order)

# Renombrar columnas h, H, m, M a Hombre y Mujer si existen
colnames(fallecimientos_edad_sexo) <- colnames(fallecimientos_edad_sexo) %>%
  gsub('^(h|H)$', 'Hombre', .) %>%
  gsub('^(m|M)$', 'Mujer', .)

# Calcular columna Total por fila (edad)
fallecimientos_edad_sexo <- fallecimientos_edad_sexo %>%
  mutate(Total = rowSums(select(., -edad)))

# Calcular total general
total_general <- sum(fallecimientos_edad_sexo$Total, na.rm = TRUE)

# Calcular % de cada celda respecto al total general
fallecimientos_edad_sexo <- fallecimientos_edad_sexo %>%
  mutate(
    pct_Hombre = round(Hombre / total_general * 100, 2),
    pct_Mujer = round(Mujer / total_general * 100, 2),
    pct_Total = round(Total / total_general * 100, 2)
  )

# Añadir fila total nacional y que los % sean la suma de los % de cada fila
fila_total <- fallecimientos_edad_sexo[1, ]
fila_total[1, ] <- NA
fila_total$edad <- "Total"
for (col in names(fila_total)) {
  if (col %in% c("Hombre", "Mujer", "Total")) {
    fila_total[[col]] <- sum(fallecimientos_edad_sexo[[col]], na.rm = TRUE)
  }
}
fila_total$pct_Hombre <- sum(fallecimientos_edad_sexo$pct_Hombre, na.rm = TRUE)
fila_total$pct_Mujer  <- sum(fallecimientos_edad_sexo$pct_Mujer, na.rm = TRUE)
fila_total$pct_Total  <- sum(fallecimientos_edad_sexo$pct_Total, na.rm = TRUE)
fallecimientos_edad_sexo <- bind_rows(fallecimientos_edad_sexo, fila_total)

## Tabla gt
t26 <- fallecimientos_edad_sexo %>%
  gt() %>%
  tab_header(
    title = "Fallecimientos durante tramitación por sexo y edad",
    subtitle = "Número y porcentaje por grupo de edad y totales"
  ) %>%
  cols_label(
    edad = "Edad",
    Hombre = "Num",
    pct_Hombre = "%",
    Mujer = "Num",
    pct_Mujer = "%",
    Total = "Num",
    pct_Total = "%"
  ) %>%
  cols_align(
    align = "center",
    columns = -edad
  ) %>%
  tab_spanner(
    label = "Hombre",
    columns = c(Hombre, pct_Hombre),
    id = "spanner_hombre"
  ) %>%
  tab_spanner(
    label = "Mujer",
    columns = c(Mujer, pct_Mujer),
    id = "spanner_mujer"
  ) %>%
  tab_spanner(
    label = "Total",
    columns = c(Total, pct_Total),
    id = "spanner_total"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = edad == "Total"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Total, pct_Total)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = c(Total, pct_Total)
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t26, "script_tablas/tablas_def/tabla_26.html")

### Tabla 27: Fallecimientos durante tramitación por enfermedad de base (n, solicitudes y tasa) 2024
tabla27_base <- df %>%
  mutate(
    patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia)
  ) %>%
  group_by(patologia) %>%
  summarise(
    Solicitudes = n(),
    Fallecimientos_base = sum(fallecimiento_tramitacion_enf_base == "Si", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Tasa_fallecimiento = ifelse(Solicitudes == 0, NA, round(100 * Fallecimientos_base / Solicitudes, 2))
  )

# Añadir fila total nacional
fila_total_27 <- tibble(
  patologia = "Total",
  Solicitudes = sum(tabla27_base$Solicitudes, na.rm = TRUE),
  Fallecimientos_base = sum(tabla27_base$Fallecimientos_base, na.rm = TRUE),
  Tasa_fallecimiento = round(100 * sum(tabla27_base$Fallecimientos_base, na.rm = TRUE) / sum(tabla27_base$Solicitudes, na.rm = TRUE), 2)
)

tabla27 <- bind_rows(tabla27_base, fila_total_27) %>%
  mutate(order = if_else(patologia == "Total", 2L, 0L)) %>%
  arrange(order, patologia) %>%
  select(-order)

# Etiquetas para gt
labels_27 <- list(
  patologia = "Enfermedad de base",
  Fallecimientos_base = "Núm",
  Solicitudes = "Solicitudes",
  Tasa_fallecimiento = "Tasa de fallecimiento por enf. base (%)"
)

# Crear tabla gt
t27 <- tabla27 %>%
  gt() %>%
  tab_header(
    title = "Fallecimientos por enfermedad de base",
    subtitle = "Núm, solicitudes y tasa de fallecimiento (%) por enfermedad de base"
  ) %>%
  cols_label(.list = labels_27) %>%
  cols_align(
    align = "center",
    columns = -patologia
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
    source_note = "Nota: Elaboración propia."
  )

gtsave(t27, "script_tablas/tablas_def/tabla_27.html")

### Tabla 28: Tiempos del procedimiento por CC. AA. 2024 (media y mediana)
df_tiempos <- df %>%
  mutate(
    tiempo_1sol_prestacion = ifelse(
      !is.na(fecha_prestacion) & !is.na(fecha_primera_solicitud), #& fecha_prestacion >= fecha_primera_solicitud,
      as.numeric(fecha_prestacion - fecha_primera_solicitud),
      NA
    ),
    tiempo_1sol_2sol = ifelse(
      !is.na(fecha_segunda_solicitud) & !is.na(fecha_primera_solicitud), #& fecha_segunda_solicitud >= fecha_primera_solicitud,
      as.numeric(fecha_segunda_solicitud - fecha_primera_solicitud),
      NA
    ),
    tiempo_2sol_mc = ifelse(
      !is.na(fecha_informe_mc) & !is.na(fecha_segunda_solicitud), # & fecha_informe_mc >= fecha_segunda_solicitud,
      as.numeric(fecha_informe_mc - fecha_segunda_solicitud),
      NA
    ),
    tiempo_mc_cgye = ifelse(
      !is.na(fecha_informe_cgye) & !is.na(fecha_informe_mc), # & fecha_informe_cgye >= fecha_informe_mc,
      as.numeric(fecha_informe_cgye - fecha_informe_mc),
      NA
    ),
    tiempo_cgye_prestacion = ifelse(
      !is.na(fecha_prestacion) & !is.na(fecha_informe_cgye), # & fecha_prestacion >= fecha_informe_cgye,
      as.numeric(fecha_prestacion - fecha_informe_cgye),
      NA
    )
  )

# Calcular medias y medianas por CCAA
tabla_tiempos_ccaa <- df_tiempos %>%
  group_by(ccaa) %>%
  summarise(
    `1ª Solicitud a Prestación - Media` = round(mean(tiempo_1sol_prestacion, na.rm = TRUE), 2),
    `1ª Solicitud a Prestación - Mediana` = round(median(tiempo_1sol_prestacion, na.rm = TRUE), 2),
    `1ª a 2ª Solicitud - Media` = round(mean(tiempo_1sol_2sol, na.rm = TRUE), 2),
    `1ª a 2ª Solicitud - Mediana` = round(median(tiempo_1sol_2sol, na.rm = TRUE), 2),
    `2ª Solicitud a MC - Media` = round(mean(tiempo_2sol_mc, na.rm = TRUE), 2),
    `2ª Solicitud a MC - Mediana` = round(median(tiempo_2sol_mc, na.rm = TRUE), 2),
    `MC a CGyE - Media` = round(mean(tiempo_mc_cgye, na.rm = TRUE), 2),
    `MC a CGyE - Mediana` = round(median(tiempo_mc_cgye, na.rm = TRUE), 2),
    `CGyE a Prestación - Media` = round(mean(tiempo_cgye_prestacion, na.rm = TRUE), 2),
    `CGyE a Prestación - Mediana` = round(median(tiempo_cgye_prestacion, na.rm = TRUE), 2)
  ) %>%
  ungroup()

# Calcular totales nacionales (media y mediana de cada columna)
fila_total <- tibble(
  ccaa = "Total nacional",
  `1ª Solicitud a Prestación - Media` = round(mean(df_tiempos$tiempo_1sol_prestacion, na.rm = TRUE), 2),
  `1ª Solicitud a Prestación - Mediana` = round(median(df_tiempos$tiempo_1sol_prestacion, na.rm = TRUE), 2),
  `1ª a 2ª Solicitud - Media` = round(mean(df_tiempos$tiempo_1sol_2sol, na.rm = TRUE), 2),
  `1ª a 2ª Solicitud - Mediana` = round(median(df_tiempos$tiempo_1sol_2sol, na.rm = TRUE), 2),
  `2ª Solicitud a MC - Media` = round(mean(df_tiempos$tiempo_2sol_mc, na.rm = TRUE), 2),
  `2ª Solicitud a MC - Mediana` = round(median(df_tiempos$tiempo_2sol_mc, na.rm = TRUE), 2),
  `MC a CGyE - Media` = round(mean(df_tiempos$tiempo_mc_cgye, na.rm = TRUE), 2),
  `MC a CGyE - Mediana` = round(median(df_tiempos$tiempo_mc_cgye, na.rm = TRUE), 2),
  `CGyE a Prestación - Media` = round(mean(df_tiempos$tiempo_cgye_prestacion, na.rm = TRUE), 2),
  `CGyE a Prestación - Mediana` = round(median(df_tiempos$tiempo_cgye_prestacion, na.rm = TRUE), 2)
)

tabla_tiempos_ccaa <- bind_rows(tabla_tiempos_ccaa, fila_total)

# Formatear tabla con gt
t28 <- tabla_tiempos_ccaa %>%
  gt(rowname_col = "ccaa") %>%
  tab_header(
    title = "Tiempos (en días) entre hitos del proceso por CCAA",
    subtitle = "Media y mediana de los intervalos principales del proceso"
  ) %>%
  tab_spanner(
    label = "1ª Solicitud a Prestación",
    columns = c("1ª Solicitud a Prestación - Media", "1ª Solicitud a Prestación - Mediana")
  ) %>%
  tab_spanner(
    label = "1ª a 2ª Solicitud",
    columns = c("1ª a 2ª Solicitud - Media", "1ª a 2ª Solicitud - Mediana")
  ) %>%
  tab_spanner(
    label = "2ª Solicitud a MC",
    columns = c("2ª Solicitud a MC - Media", "2ª Solicitud a MC - Mediana")
  ) %>%
  tab_spanner(
    label = "MC a CGyE",
    columns = c("MC a CGyE - Media", "MC a CGyE - Mediana")
  ) %>%
  tab_spanner(
    label = "CGyE a Prestación",
    columns = c("CGyE a Prestación - Media", "CGyE a Prestación - Mediana")
  ) %>%
  cols_label(
    ccaa = "CCAA",
    `1ª Solicitud a Prestación - Media` = "Media",
    `1ª Solicitud a Prestación - Mediana` = "Mediana",
    `1ª a 2ª Solicitud - Media` = "Media",
    `1ª a 2ª Solicitud - Mediana` = "Mediana",
    `2ª Solicitud a MC - Media` = "Media",
    `2ª Solicitud a MC - Mediana` = "Mediana",
    `MC a CGyE - Media` = "Media",
    `MC a CGyE - Mediana` = "Mediana",
    `CGyE a Prestación - Media` = "Media",
    `CGyE a Prestación - Mediana` = "Mediana"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = px(13),
    data_row.padding = px(3)
  )

gtsave(t28, "script_tablas/tablas_def/tabla_28.html")

### Tabla 29: Días entre reclamación y resolución de la CGyE por CCAA (media y mediana)
df_tiempos <- df %>%
  mutate(
    tiempo_reclamacion_resolucion = ifelse(
      !is.na(fecha_reclamacion_cgye) & !is.na(fecha_resolucion_cgye) & fecha_resolucion_cgye >= fecha_reclamacion_cgye,
      as.numeric(fecha_resolucion_cgye - fecha_reclamacion_cgye),
      NA
    )
  )

# Calcular medias y medianas por CCAA
tabla_tiempos_ccaa <- df_tiempos %>%
  group_by(ccaa) %>%
  summarise(
    Media = round(mean(tiempo_reclamacion_resolucion, na.rm = TRUE), 2),
    Mediana = round(median(tiempo_reclamacion_resolucion, na.rm = TRUE), 2)
  ) %>%
  ungroup()

# Añadir fila total nacional
fila_total <- tibble(
  ccaa = "Total nacional",
  Media = round(mean(df_tiempos$tiempo_reclamacion_resolucion, na.rm = TRUE), 2),
  Mediana = round(median(df_tiempos$tiempo_reclamacion_resolucion, na.rm = TRUE), 2)
)

tabla_tiempos_ccaa <- bind_rows(tabla_tiempos_ccaa, fila_total)

# Formatear tabla con gt
t29 <- tabla_tiempos_ccaa %>%
  gt(rowname_col = "ccaa") %>%
  tab_header(
    title = "Días entre reclamación y resolución de la CGyE por CCAA",
    subtitle = "Media y mediana de los días entre reclamación y resolución"
  ) %>%
  cols_label(
    ccaa = "CCAA",
    Media = "Media",
    Mediana = "Mediana"
  ) %>%
  fmt_number(
    columns = c(Media, Mediana),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = px(13),
    data_row.padding = px(3)
  )

gtsave(t29, "script_tablas/tablas_def/tabla_29.html")

### Tabla 30: Tiempos del procedimiento 2021-2024 (media y mediana)
df_tiempos <- df %>%
  mutate(
    tiempo_1sol_prestacion = ifelse(
      !is.na(fecha_prestacion) & !is.na(fecha_primera_solicitud) & fecha_prestacion >= fecha_primera_solicitud,
      as.numeric(fecha_prestacion - fecha_primera_solicitud),
      NA
    ),
    tiempo_1sol_2sol = ifelse(
      !is.na(fecha_segunda_solicitud) & !is.na(fecha_primera_solicitud) & fecha_segunda_solicitud >= fecha_primera_solicitud,
      as.numeric(fecha_segunda_solicitud - fecha_primera_solicitud),
      NA
    ),
    tiempo_2sol_mc = ifelse(
      !is.na(fecha_informe_mc) & !is.na(fecha_segunda_solicitud) & fecha_informe_mc >= fecha_segunda_solicitud,
      as.numeric(fecha_informe_mc - fecha_segunda_solicitud),
      NA
    ),
    tiempo_mc_cgye = ifelse(
      !is.na(fecha_informe_cgye) & !is.na(fecha_informe_mc) & fecha_informe_cgye >= fecha_informe_mc,
      as.numeric(fecha_informe_cgye - fecha_informe_mc),
      NA
    ),
    tiempo_cgye_prestacion = ifelse(
      !is.na(fecha_prestacion) & !is.na(fecha_informe_cgye) & fecha_prestacion >= fecha_informe_cgye,
      as.numeric(fecha_prestacion - fecha_informe_cgye),
      NA
    ),
    tiempo_reclamacion_resolucion = ifelse(
      !is.na(fecha_reclamacion_cgye) & !is.na(fecha_resolucion_cgye) & fecha_resolucion_cgye >= fecha_reclamacion_cgye,
      as.numeric(fecha_resolucion_cgye - fecha_reclamacion_cgye),
      NA
    )
  )
  # Calcular medias y medianas de cada variable de tiempo
tabla_tiempos <- tibble::tibble(
  Variable = c(
    "tiempo_1sol_prestacion",
    "tiempo_1sol_2sol",
    "tiempo_2sol_mc",
    "tiempo_mc_cgye",
    "tiempo_cgye_prestacion",
    "tiempo_reclamacion_resolucion"
  ),
  Media = c(
    round(mean(df_tiempos$tiempo_1sol_prestacion, na.rm = TRUE), 2),
    round(mean(df_tiempos$tiempo_1sol_2sol, na.rm = TRUE), 2),
    round(mean(df_tiempos$tiempo_2sol_mc, na.rm = TRUE), 2),
    round(mean(df_tiempos$tiempo_mc_cgye, na.rm = TRUE), 2),
    round(mean(df_tiempos$tiempo_cgye_prestacion, na.rm = TRUE), 2),
    round(mean(df_tiempos$tiempo_reclamacion_resolucion, na.rm = TRUE), 2)
  ),
  Mediana = c(
    round(median(df_tiempos$tiempo_1sol_prestacion, na.rm = TRUE), 2),
    round(median(df_tiempos$tiempo_1sol_2sol, na.rm = TRUE), 2),
    round(median(df_tiempos$tiempo_2sol_mc, na.rm = TRUE), 2),
    round(median(df_tiempos$tiempo_mc_cgye, na.rm = TRUE), 2),
    round(median(df_tiempos$tiempo_cgye_prestacion, na.rm = TRUE), 2),
    round(median(df_tiempos$tiempo_reclamacion_resolucion, na.rm = TRUE), 2)
  )
)

# Mostrar la tabla
print(tabla_tiempos)

# Hacer tabla gt con el resto de datos manuales
tabla_30 <- tibble::tibble(
   LORE = c(
    "Entre la 1ª solicitud y prestación",
    "Entre la 1ª y la 2ª solicitud",
    "Entre la 2ª solicitud y el informe del MC",
    "Entre el informe del MC y la resolución de la CGyE",
    "Entre la resolución favorable de la CGyE y la prestación",
    "Entre la reclamación y la resolución de la CGyE"
  ),
  Referencia_LORE = c(
    "No establecido*",
    "Al menos 15 días",
    "Máximo 10 días naturales",
    "14 días",
    "Acordado por MR y paciente",
    "20 días naturales"
  ),
   Media_2021 = c(60, 21, 13, 13, 25, 19),
  Media_2022 = c(75, 26, 13, 12, 23, 23),
  Mediana_2022 = c(55.50, 16.50, 8, 11, 13, 19.50),
  Media_2023 = c(67, 22, 12.26, 14.70, 20.26, 24.25),
  Mediana_2023 = c(54.20, 17.35, 9.26, 11.64, 12.82, 20.88),
  Media_2024 = c(82.60, 21.70, 17.50, 15.10, 27.40, 25.50),
  Mediana_2024 = c(62, 16, 9, 10, 15, 19)
)

labels_30 <- c(
  Referencia_LORE = "Días según LORE",
  LORE = "",
  Media_2021 = "Media",
  Media_2022 = "Media",
  Mediana_2022 = "Mediana",
  Media_2023 = "Media",
  Mediana_2023 = "Mediana",
  Media_2024 = "Media",
  Mediana_2024 = "Mediana"
)

t30 <- tabla_30 %>%
  gt() %>%
  tab_header(
    title = "Tiempos de procedimiento, histórico 2021-2024"
  ) %>%
  cols_label(.list = labels_30) %>%
  cols_align(
    align = "center",
    columns = -Referencia_LORE
  ) %>%
  tab_spanner(
    label = "2021",
    columns = c("Media_2021"),
    id = "spanner_2021"
  ) %>%
  tab_spanner(
    label = "2022",
    columns = c("Media_2022", "Mediana_2022"),
    id = "spanner_2022"
  ) %>%
  tab_spanner(
    label = "2023",
    columns = c("Media_2023", "Mediana_2023"),
    id = "spanner_2023"
  ) %>%
  tab_spanner(
    label = "2024",
    columns = c("Media_2024", "Mediana_2024"),
    id = "spanner_2024"
  ) %>%
    tab_source_note(
    source_note = "* Este plazo no está establecido como tal en la LORE. Se puede realizar una estimación aproximada de un plazo entre 30 y 40 días para la resolución de la CGyE de acuerdo con los plazos mínimos y máximos establecidos en la Ley. Una vez que la CGyE ha resuelto favorablemente, el plazo es muy variable puesto que el solicitante dispone de un margen de flexibilidad de hasta 2 meses para recibir la prestación."
  )

gtsave(t30, "script_tablas/tablas_def/tabla_30.html")


###Tabla 31: Solicitudes, solicitudes aprobadas, aplazamientos y tasa de aplazamientos por CC.AA 2024.
tabla_31 <- df %>%
  group_by(ccaa) %>%
  summarise(
    Solicitudes = n(),
    Solicitudes_aprobadas = sum(informe_cgye == "Si" | resolucion_fav_cgye == "Si", na.rm = TRUE),
    Aplazamientos = sum(aplazamiento == "Si", na.rm = TRUE)
  ) %>%
  mutate(
    Tasa_aplazamientos_aprobadas = round(Aplazamientos / ifelse(Solicitudes_aprobadas == 0, NA, Solicitudes_aprobadas) * 100, 2)
  )

# Añadir fila total
fila_total <- tibble::tibble(
  ccaa = "Total",
  Solicitudes = sum(tabla_30$Solicitudes, na.rm = TRUE),
  Solicitudes_aprobadas = sum(tabla_30$Solicitudes_aprobadas, na.rm = TRUE),
  Aplazamientos = sum(tabla_30$Aplazamientos, na.rm = TRUE),
  Tasa_aplazamientos_aprobadas = round(sum(tabla_30$Aplazamientos, na.rm = TRUE) / ifelse(sum(tabla_30$Solicitudes_aprobadas, na.rm = TRUE) == 0, NA, sum(tabla_30$Solicitudes_aprobadas, na.rm = TRUE)) * 100, 2)
)
tabla_31 <- bind_rows(tabla_31, fila_total)

# Etiquetas para gt
labels_31 <- c(
  ccaa = "CC. AA.",
  Solicitudes = "Solicitudes",
  Solicitudes_aprobadas = "Solicitudes aprobadas",
  Aplazamientos = "Aplazamientos",
  Tasa_aplazamientos_aprobadas = "Tasa de aplazamientos por solicitudes aprobadas (%)"
)

t31 <- tabla_31 %>%
  gt() %>%
  tab_header(
    title = "Solicitudes, solicitudes aprobadas, aplazamientos y tasa de aplazamientos por CC.AA. 2024"
  ) %>%
  cols_label(.list = labels_31) %>%
  cols_align(
    align = "center",
    columns = -ccaa
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = ccaa == "Total"
    )
  )

gtsave(t31, "script_tablas/tablas_def/tabla_31.html")


### Tabla 32: Aplazamientos por sexo y edad (n y %), 2024
niveles_edad <- c("<30", "30-39", "40-49", "50-59", "60-69", "70-79", ">80")

aplazamientos_edad_sexo <- df %>%
  filter(aplazamiento == "Si") %>%
  mutate(
    sexo = ifelse(is.na(sexo) | sexo == "", "No consta", sexo)
  ) %>%
  group_by(edad, sexo) %>%
  summarise(n = n(), .groups = 'drop') %>%
  complete(edad = niveles_edad, sexo, fill = list(n = 0)) %>%
  tidyr::pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
  mutate(order = case_when(
    edad == "<30" ~ 0,
    edad == "30-39" ~ 1,
    edad == "40-49" ~ 2,
    edad == "50-59" ~ 3,
    edad == "60-69" ~ 4,
    edad == "70-79" ~ 5,
    edad == ">80" ~ 6,
    edad == "Total" ~ 7,
    edad == "No consta" ~ 8,
    TRUE ~ 99
  )) %>%
  arrange(order) %>%
  select(-order)


# Renombrar columnas h, H, m, M a Hombre y Mujer si existen
colnames(aplazamientos_edad_sexo) <- colnames(aplazamientos_edad_sexo) %>%
  gsub('^(h|H)$', 'Hombre', .) %>%
  gsub('^(m|M)$', 'Mujer', .)

# Calcular columna Total por fila (edad)
aplazamientos_edad_sexo <- aplazamientos_edad_sexo %>%
  mutate(Total = rowSums(select(., -edad)))

# Calcular % por sexo y total respecto al total de aplazamientos
total_hombres <- sum(aplazamientos_edad_sexo$Hombre, na.rm = TRUE)
total_mujeres <- sum(aplazamientos_edad_sexo$Mujer, na.rm = TRUE)
total_aplazamientos <- sum(aplazamientos_edad_sexo$Total, na.rm = TRUE)

aplazamientos_edad_sexo <- aplazamientos_edad_sexo %>%
  mutate(
    pct_Hombre = round(Hombre / total_aplazamientos * 100, 2),
    pct_Mujer = round(Mujer / total_aplazamientos * 100, 2),
    pct_Total = round(Total / total_aplazamientos * 100, 2)
  )

# Añadir fila total nacional
fila_total <- aplazamientos_edad_sexo[1, ]
fila_total[1, ] <- NA
fila_total$edad <- "Total"
for (col in names(fila_total)) {
  if (col %in% c("Hombre", "Mujer", "Total")) {
    fila_total[[col]] <- sum(aplazamientos_edad_sexo[[col]], na.rm = TRUE)
  }
}
fila_total$pct_Hombre <- round(fila_total$Hombre / total_aplazamientos * 100, 2)
fila_total$pct_Mujer  <- round(fila_total$Mujer  / total_aplazamientos * 100, 2)
fila_total$pct_Total  <- round(fila_total$Total  / total_aplazamientos * 100, 2)
aplazamientos_edad_sexo <- bind_rows(aplazamientos_edad_sexo, fila_total)

# Seleccionar y renombrar columnas para gt
cols_gt <- c("edad",
             "Hombre", "pct_Hombre",
             "Mujer", "pct_Mujer",
             "Total", "pct_Total")
aplazamientos_edad_gt <- aplazamientos_edad_sexo[, cols_gt]

# Formatear y guardar tabla con gt
t32 <- aplazamientos_edad_gt %>%
  gt() %>%
  tab_header(
    title = "Aplazamientos por sexo y edad",
    subtitle = "Número y porcentaje por grupo de edad y totales"
  ) %>%
  cols_label(
    edad = "Edad",
    Hombre = "Num",
    pct_Hombre = "%",
    Mujer = "Num",
    pct_Mujer = "%",
    Total = "Num",
    pct_Total = "%"
  ) %>%
  cols_align(
    align = "center",
    columns = -edad
  ) %>%
  tab_spanner(
    label = "Hombre",
    columns = c("Hombre", "pct_Hombre"),
    id = "spanner_hombre"
  ) %>%
  tab_spanner(
    label = "Mujer",
    columns = c("Mujer", "pct_Mujer"),
    id = "spanner_mujer"
  ) %>%
  tab_spanner(
    label = "Total",
    columns = c("Total", "pct_Total"),
    id = "spanner_total"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = edad == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t32, "script_tablas/tablas_def/tabla_32.html")

### Tabla 33: Aplazamientos durante tramitación por enfermedad de base (n, solicitudes y tasa) 2024
tabla33_base <- df %>%
  mutate(
    patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia)
  ) %>%
  group_by(patologia) %>%
  summarise(
    Solicitudes = n(),
    Aplazamientos = sum(aplazamiento == "Si", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Tasa_aplazamiento = ifelse(Solicitudes == 0, NA, round(100 * Aplazamientos / Solicitudes, 2))
  )

# Añadir fila total nacional
fila_total_33 <- tibble(
  patologia = "Total",
  Solicitudes = sum(tabla33_base$Solicitudes, na.rm = TRUE),
  Aplazamientos = sum(tabla33_base$Aplazamientos, na.rm = TRUE),
  Tasa_aplazamiento = round(100 * sum(tabla33_base$Aplazamientos, na.rm = TRUE) / sum(tabla33_base$Solicitudes, na.rm = TRUE), 2)
)

tabla33 <- bind_rows(tabla33_base, fila_total_33) %>%
  mutate(order = if_else(patologia == "Total", 2L, 0L)) %>%
  arrange(order, patologia) %>%
  select(-order)

# Etiquetas para gt
labels_33 <- list(
  patologia = "Enfermedad de base",
  Aplazamientos= "Núm",
  Solicitudes = "Solicitudes",
  Tasa_aplazamiento = "Tasa de aplazamiento por enf. base (%)"
)

# Crear tabla gt
t33 <- tabla33 %>%
  gt() %>%
  tab_header(
    title = "Aplazamientos por enfermedad de base",
    subtitle = "Núm, solicitudes y tasa de aplazamiento (%) por enfermedad de base"
  ) %>%
  cols_label(.list = labels_33) %>%
  cols_align(
    align = "center",
    columns = -patologia
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
    source_note = "Nota: Elaboración propia."
  )

gtsave(t33, "script_tablas/tablas_def/tabla_33.html")

### Tabla 34: Tasa de aplazamientos frente a solicitudes totales (n y %), histórico 2021-2024
tabla_34 <- tibble::tibble(
  Año = c("2021", "2022", "2023", "2024"),
  Aplazamientos_totales = c(6, 22, 33, 50),  
  Solicitudes_totales = c(173, 576, 766, 917)   
)

# Calcular tasa de aplazamientos por año
tabla_34 <- tabla_34 %>%
  mutate(
    Tasa_aplazamientos = round(Aplazamientos_totales / Solicitudes_totales * 100, 2)
  )

# Añadir fila total
fila_total_34 <- tibble::tibble(
  Año = "Total",
  Aplazamientos_totales = sum(tabla_34$Aplazamientos_totales, na.rm = TRUE),
  Solicitudes_totales = sum(tabla_34$Solicitudes_totales, na.rm = TRUE),
  Tasa_aplazamientos = round(sum(tabla_34$Aplazamientos_totales, na.rm = TRUE) / sum(tabla_34$Solicitudes_totales, na.rm = TRUE) * 100, 2)
)
tabla_34 <- dplyr::bind_rows(tabla_34, fila_total_34)

# Etiquetas para gt
labels_34 <- c(
  Año = "Año",
  Aplazamientos_totales = "Aplazamientos totales",
  Solicitudes_totales = "Solicitudes totales",
  Tasa_aplazamientos = "Tasa de aplazamientos (%)"
)

t34 <- tabla_34 %>%
  gt() %>%
  tab_header(
    title = "Tasa de aplazamientos frente a solicitudes totales",
    subtitle = "Num y %, histórico 2021-2024"
  ) %>%
  cols_label(.list = labels_34) %>%
  cols_align(
    align = "center",
    columns = -Año
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = Año == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t34, "script_tablas/tablas_def/tabla_34.html")


### Tabla 35: Tasa de acortamientos por solicitudes, histórico 2022-2024 (transpuesta)
tabla_35 <- tibble::tibble(
  Indicador = c("Solicitudes totales", "Acortamientos", "Tasa de acortamientos por solicitudes (%)"),
  `2022` = c(576, 82, round(82 / 576 * 100, 2)),
  `2023` = c(68, 766, round(68 / 766 * 100, 2)),
  `2024` = c(117, 917, round(117 / 917 * 100, 2))
)

t35 <- tabla_35 %>%
  gt() %>%
  tab_header(
    title = "Tasa de acortamientos por solicitudes, histórico 2022-2024"
  ) %>%
  cols_label(
    Indicador = "",
    `2022` = "2022",
    `2023` = "2023",
    `2024` = "2024"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = Indicador == "Tasa de acortamientos por solicitudes (%)"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t35, "script_tablas/tablas_def/tabla_35.html")

### Tabla 36: Frecuencia de Donaciones, extracciones y trasplantes de órganos tras la PAM por años 2021-24
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
t36 <- df_donantes_organos_año_t %>%
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

gtsave(t36, "script_tablas/tablas_def/tabla_36.html")

# (AP. 5.3.) Figura 10: Prestaciones por ámbito de prestación público-privado y CC. AA. 2024 (barras apiladas por CC. AA.)
ambito<-c("Público", "Privado")
porc<-c("96.68", "3.32")
fig10 <- data.frame(ambito, porc)

ggplot(fig10, aes(x = "", y = as.numeric(porc), fill = ambito)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("Público" = "#fdbb63", "Privado" = "#6b174a")) +
    theme_void() +
    labs(title = "Prestaciones por ámbito de prestación público-privado 2024", fill = "Ámbito") +
    geom_text(aes(label = paste0(porc, "%")), 
              position = position_stack(vjust = 0.5), 
              color = "black", size = 7) +
    theme(
        plot.title = element_text(size = 20, margin = margin(l = -10), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(t=20, b = 20),
    ) +
    geom_label(
        aes(label = paste0(porc, "%"), y = as.numeric(porc), fill = ambito),
        color = "black",
        fill = "white",
        label.size = 0.7,
        size = 7,
        show.legend = FALSE,
        position = position_stack(vjust = 0.5)
    )

ggsave("script_figuras/figuras_def/fig10.png", width = 8, height = 8, dpi = 300, bg = "transparent")