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
solicitudes_historico <- data.frame(
  Comunidad = c("Andalucía", "Aragón", "Asturias", "Canarias", "Cantabria",
                "Castilla-La Mancha", "Castilla y León", "Cataluña", "Comunidad Valenciana",
                "Extremadura", "Galicia", "Islas Baleares", "Madrid", "Murcia", "Navarra", "País Vasco",
                "La Rioja", "Ceuta", "Melilla"),
  `2021` = c(3, 4, 4, 8, 2, 1, 4, 65, 10, 1, 6, 9, 12, 3, 4, 34, 2, 0, 1),
  `2022` = c(46, 26, 19, 21, 17, 20, 25, 175, 46, 11, 17, 22, 62, 5, 19, 40, 5, 0, 0),
  `2023` = c(43, 22, 33, 62, 19, 28, 27, 219, 56, 2, 41, 37, 89, 2, 24, 58, 4, 0, 0),
  `2024` = c(72, 22, 34, 52, 13, 26, 40, 351, 30, 8, 33, 48, 130, 9, 23, 76, 9, 0, 1)
)

solicitudes_largo <- solicitudes_historico %>%
  pivot_longer(cols = -Comunidad, names_to = "Año", values_to = "N")

# Calcular total nacional por año
totales <- solicitudes_largo %>%
  group_by(Año) %>%
  summarise(N = sum(N, na.rm = TRUE)) %>%
  rename(Total_Nacional = N)

# Unir para calcular el %
solicitudes_largo <- solicitudes_largo %>%
  left_join(totales, by = "Año") %>%
  mutate(Porcentaje = round((N / Total_Nacional) * 100, 2))

# Volver a formato ancho: una fila por comunidad, columnas n y % por año
solicitudes_ancho <- solicitudes_largo %>%
  select(Comunidad, Año, N, Porcentaje) %>%
  pivot_wider(
    names_from = Año,
    values_from = c(N, Porcentaje),
    names_glue = "{Año}_{.value}"
  )

# Construir fila total nacional con los mismos nombres y orden que solicitudes_ancho
fila_total <- solicitudes_ancho[1, ]
fila_total[1, ] <- NA
fila_total$Comunidad <- "Total"
for (col in names(fila_total)) {
  if (grepl("_N$", col)) {
    anio <- sub("_N$", "", col)
    fila_total[[col]] <- totales$Total_Nacional[totales$Año == anio]
  } else if (grepl("_Porcentaje$", col)) {
    fila_total[[col]] <- 100
  }
}

solicitudes_tabla <- bind_rows(solicitudes_ancho, fila_total)

# Ordenar para que Total quede al final
solicitudes_tabla <- solicitudes_tabla %>%
  mutate(order = if_else(Comunidad == "Total", 2L, 0L)) %>%
  arrange(order, Comunidad) %>%
  select(-order)

# Crear tabla
  t3 <- solicitudes_tabla %>%
    gt() %>%
    tab_header(
      title = "Solicitudes por Comunidad Autónoma y año",
      subtitle = "Núm. y % sobre el total nacional"
    ) %>%
    cols_label(
      Comunidad = "Comunidad Autónoma",
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
      source_note = "Nota: Elaboración propia.")
  

gtsave(t3, "script_tablas/tablas_def/tabla_3.html")


### Tabla 4: Solicitudes por sexo y edad (n) 2024
solicitudes_edad_sexo <- df %>%
  mutate(
    sexo = ifelse(is.na(sexo) | sexo == "", "No consta", sexo)
  ) %>%
  group_by(edad, sexo) %>%
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = sexo, values_from = n, values_fill = 0)%>%
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

# Calcular % de cada celda respecto al total general
total_hombres <- sum(solicitudes_edad_sexo$Hombre, na.rm = TRUE)
total_mujeres <- sum(solicitudes_edad_sexo$Mujer, na.rm = TRUE)
total_general <- sum(solicitudes_edad_sexo$Total, na.rm = TRUE)

solicitudes_edad_sexo <- solicitudes_edad_sexo %>%
  mutate(
    pct_Hombre = round(Hombre / total_hombres * 51.69, 2),
    pct_Mujer = round(Mujer / total_mujeres * 48.31, 2)
  )

# Añadir fila total nacional

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
    Total = "Total"
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
      columns = Total
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = Total
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t4, "script_tablas/tablas_def/tabla_4.html")

### Tabla 5: Solicitudes de PAM por tramo de edad (n y %), histórico 2021-2024
solicitudes_edad_historico <- data.frame(
  Edad = c("<30", "30-39", "40-49", "50-59", "60-69", "70-79", ">80", "No consta"),
  `2021` = c(1, 0, 10, 15, 16, 20, 13, 98),
  `2022` = c(4, 13, 36, 68, 121, 122, 97, 67),
  `2023` = c(7, 19, 57, 106, 181, 215, 181, 0),
  `2024` = c(12, 29, 61, 135, 211, 254, 275, 0)
)

solicitudes_edad_largo <- solicitudes_edad_historico %>%
  pivot_longer(cols = -Edad, names_to = "Año", values_to = "N")
# Calcular total nacional por año
totales_edad <- solicitudes_edad_largo %>%
  group_by(Año) %>%
  summarise(N = sum(N, na.rm = TRUE)) %>%
  rename(Total_Nacional = N)

# Unir para calcular el %
solicitudes_edad_largo <- solicitudes_edad_largo %>%
  left_join(totales_edad, by = "Año") %>%
  mutate(Porcentaje = round((N / Total_Nacional) * 100, 2)) 
# Volver a formato ancho: una fila por edad, columnas n y % por año
solicitudes_edad_ancho <- solicitudes_edad_largo %>%
  select(Edad, Año, N, Porcentaje) %>%
  pivot_wider(
    names_from = Año,
    values_from = c(N, Porcentaje),
    names_glue = "{Año}_{.value}"
  )
# Construir fila total nacional con los mismos nombres y orden que solicitudes_edad_ancho
fila_total_edad <- solicitudes_edad_ancho[1, ]
fila_total_edad[1, ] <- NA  
fila_total_edad$Edad <- "Total"
for (col in names(fila_total_edad)) {
  if (grepl("_N$", col)) {
    anio <- sub("_N$", "", col)
    fila_total_edad[[col]] <- totales_edad$Total_Nacional[totales_edad$Año == anio]
  } else if (grepl("_Porcentaje$", col)) {
    fila_total_edad[[col]] <- 100
  }
}
solicitudes_edad_tabla <- bind_rows(solicitudes_edad_ancho, fila_total_edad)
# Ordenar para que Total quede al final
solicitudes_edad_tabla <- solicitudes_edad_tabla %>%
    mutate(order = case_when(
  Edad == "<30" ~ 0,
  Edad == "30-39" ~ 1,
  Edad == "40-49" ~ 2,
  Edad == "50-59" ~ 3,
  Edad == "60-69" ~ 4,
  Edad == "70-79" ~ 5,
  Edad == ">80" ~ 6,
  Edad == "No consta" ~ 7,
  Edad == "Total" ~ 99,   # Así Total siempre será la última
  TRUE ~ 98
)) %>%
arrange(order) %>%
select(-order)

# Crear tabla
t5 <- solicitudes_edad_tabla %>%
  gt() %>%
  tab_header(
    title = "Solicitudes de PAM por tramo de edad y año",
    subtitle = "Núm. y % sobre el total nacional"
  ) %>%
  cols_label(
    Edad = "Tramo de edad",
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
    columns = -Edad
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Edad == "Total"
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia.")

gtsave(t5, "script_tablas/tablas_def/tabla_5.html")


### Tabla 6: Solicitudes de PAM por tramo de edad y enfermedad de base (n) 2024
# Ordenar la fila <30 la primera
solicitudes_edad_pato <- df %>%
  mutate(order = case_when(
    edad == "<30" ~ 0,
    edad == "30-39" ~ 1,
    edad == "40-49" ~ 2,
    edad == "50-59" ~ 3,
    edad == "60-69" ~ 4,
    edad == "70-79" ~ 5,
    edad == ">80" ~ 6,
    edad == "Total" ~ 7,
    TRUE ~ 99
  )) %>%
  arrange(order) %>%
  select(-order)

# Reordenar columnas: "Otra" antepenúltima, "No consta" penúltima, "Total" última
cols <- names(solicitudes_edad_pato)
patologias <- setdiff(cols, c("edad", "Otra", "No consta", "Total"))
nuevo_orden <- c("edad", patologias, "Otra", "No consta", "Total")
solicitudes_edad_pato <- solicitudes_edad_pato[, nuevo_orden]

# Transponer la tabla: filas pasan a columnas y columnas a filas
solicitudes_edad_pato_long <- solicitudes_edad_pato %>%
  tidyr::pivot_longer(
    cols = -edad,
    names_to = "patologia",
    values_to = "n"
  )

solicitudes_edad_pato_wide <- solicitudes_edad_pato_long %>%
  tidyr::pivot_wider(
    names_from = edad,
    values_from = n
  )

# Reordenar filas: "Otra" antepenúltima, "No consta" penúltima, "Total" última
patologias_orden <- setdiff(solicitudes_edad_pato_wide$patologia, c("Otra", "No consta", "Total"))
patologias_orden <- c(patologias_orden, "Otra", "No consta", "Total")
solicitudes_edad_pato_wide <- solicitudes_edad_pato_wide %>%
  mutate(order = match(patologia, patologias_orden)) %>%
  arrange(order) %>%
  select(-order)

# Tabla gt con estilos para la tabla transpuesta
t6 <- solicitudes_edad_pato_wide %>%
  gt() %>%
  tab_header(
    title = "Solicitudes de PAM por enfermedad de base y tramo de edad (n) 2024"
  ) %>%
  cols_label(
    patologia = "Enfermedad de base"
    # Las edades se mostrarán con su nombre original
  ) %>%
  cols_align(
    align = "center",
    columns = -patologia
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
  # Columna Total
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Total
    )
  ) %>%
  # Encabezado de columna Total
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = Total
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t6, "script_tablas/tablas_def/tabla_6.html")


###Tabla 7: Solicitudes de PAM por enfermedad de base (n y % respecto al total del año), histórico 2021-2024.
solicitudes_pato_historico <- data.frame(
  patologia = c("Neurología", "Oncología", "Pluripatología", "Reumatología y patología osteomuscular",
                "Cardiovascular", "Respiratoria", "Otra", "No consta"),
  `2021` = c(40, 22, 4, 0, 0, 3, 3, 101),
  `2022` = c(205, 192, 40, 0, 7, 16, 68, 48),
  `2023` = c(266, 271, 49, 0, 13, 24, 105, 38),
  `2024` = c(305, 276, 42, 12, 19, 33, 87, 203)
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
t7 <- solicitudes_pato_tabla %>%
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

gtsave(t7, "script_tablas/tablas_def/tabla_7.html")



### Tabla 8: Numero de informes de MR, MC y comisión 2024
tabla8 <- tibble(
  MR_Favorable = sum(df$informe_mr == "Si", na.rm = TRUE),
  MR_Desfavorable = sum(df$informe_mr == "No", na.rm = TRUE),
  MR_Total = MR_Favorable + MR_Desfavorable,
  MC_Favorable = sum(df$informe_mc == "Si", na.rm = TRUE),
  MC_Desfavorable = sum(df$informe_mc == "No", na.rm = TRUE),
  MC_Total = MC_Favorable + MC_Desfavorable,
  Comision_Favorable = sum(df$informe_cgye == "Si", na.rm = TRUE),
  Comision_Desfavorable = sum(df$informe_cgye == "No", na.rm = TRUE),
  Comision_Total = Comision_Favorable + Comision_Desfavorable
)

tabla8 <- tabla8 %>%
  gt() %>%
  tab_header(
    title = "Número de informes de MR, MC y Comisión (n), 2024"
  ) %>%
  cols_label(
    MR_Favorable = "Favorable", MR_Desfavorable = "Desfavorable", MR_Total = "Total",
    MC_Favorable = "Favorable", MC_Desfavorable = "Desfavorable", MC_Total = "Total",
    Comision_Favorable = "Favorable", Comision_Desfavorable = "Desfavorable", Comision_Total = "Total"
  ) %>%
  tab_spanner(label = "MR", columns = c(MR_Favorable, MR_Desfavorable, MR_Total)) %>%
  tab_spanner(label = "MC", columns = c(MC_Favorable, MC_Desfavorable, MC_Total)) %>%
  tab_spanner(label = "Comisión", columns = c(Comision_Favorable, Comision_Desfavorable, Comision_Total)) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_source_note(source_note = "Nota: Elaboración propia.")

gtsave(tabla8, "script_tablas/tablas_def/tabla_8.html")

### Tabla 9: Solicitudes por país de nacimiento (n y %), 2024
tabla9 <- df %>%
  mutate(pais_nacimiento = ifelse(is.na(pais_nacimiento) | pais_nacimiento == "", "No consta", pais_nacimiento)) %>%
  group_by(pais_nacimiento) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  arrange(desc(n))

# Añadir fila total
fila_total <- data.frame(pais_nacimiento = "Total",
                        n = sum(tabla9$n),
                        pct = 100)
tabla9 <- bind_rows(tabla9, fila_total)

# Tabla gt
t9 <- tabla9 %>%
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

gtsave(t9, "script_tablas/tablas_def/tabla_9.html")

### Tabla 10: Tasa de solicitudes por 100.000 habitantes por ccaa, histórico 2021-2024

tabla10 <- data.frame(
  ccaa = c("Andalucía", "Aragón", "Asturias", "Canarias", "Cantabria",
           "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana",
           "Extremadura", "Galicia", "Islas Baleares", "Madrid", "Murcia", "Navarra", "País Vasco",
           "La Rioja", "Ceuta", "Melilla", "Total"),
  POB_2021 = c(8484804, 1331938, 1012117, 2178924, 584708,
               2385223, 2052505, 7749896, 5067911,
               1061636, 2698177, 1183415, 6726640, 1518279, 662032, 2212628,
               319444, 84071, 86450, 47400798),
  POB_2022 = c(8511167, 1328215, 1004960, 2185607, 585450,
               2375583, 2058278, 7761823, 5108116,
               1056808, 2692825, 1187043, 6743254, 1529658, 664514, 2205826,
               319617, 83051, 84932, 47486727),
  POB_2023 = c(8584147, 1341289, 1006060, 2213016, 588387,
               2383703, 2084086, 7901963, 5216195,
               1054306, 2699424, 1209906, 6871903, 1551692, 672155, 2216302,
               322282, 83052, 85493, 48085361),
  POB_2024 = c(8631862, 1351591, 1009599, 2238754, 590851,
               2391682, 2104433, 8012231, 5319285,
               1054681, 2705833, 1231768, 7009268, 1568492, 678333, 2227684,
               324184, 83179, 85985, 48619695)
) 

solicitudes_historico2 <- data.frame(
  Comunidad = c("Andalucía", "Aragón", "Asturias", "Canarias", "Cantabria",
                "Castilla-La Mancha", "Castilla y León", "Cataluña", "Comunidad Valenciana",
                "Extremadura", "Galicia", "Islas Baleares", "Madrid", "Murcia", "Navarra", "País Vasco",
                "La Rioja", "Ceuta", "Melilla", "Total"),
  SOL_2021 = c(3, 4, 4, 8, 2, 1, 4, 65, 10, 1, 6, 9, 12, 3, 4, 34, 2, 0, 1, 173),
  SOL_2022 = c(46, 26, 19, 21, 17, 20, 25, 175, 46, 11, 17, 22, 62, 5, 19, 40, 5, 0, 0, 576),
  SOL_2023 = c(43, 22, 33, 62, 19, 28, 27, 219, 56, 2, 41, 37, 89, 2, 24, 58, 4, 0, 0, 766),
  SOL_2024 = c(72, 22, 34, 52, 13, 26, 40, 351, 30, 8, 33, 48, 130, 9, 23, 76, 9, 0, 1, 977)
)

#Preparamos y renombramos columnas
solicitudes_clean <- solicitudes_historico2 %>%
  rename(ccaa = Comunidad)

# Unimos y calculamos tasas por 100.000 habitantes
tabla_tasas <- solicitudes_clean %>%
  left_join(tabla10, by = "ccaa") %>%
  transmute(
    ccaa,
    tasa_2021 = round(SOL_2021 / POB_2021 * 100000, 2),
    tasa_2022 = round(SOL_2022 / POB_2022 * 100000, 2),
    tasa_2023 = round(SOL_2023 / POB_2023 * 100000, 2),
    tasa_2024 = round(SOL_2024 / POB_2024 * 100000, 2)
  )


# Crear tabla gt
t10 <- tabla_tasas %>%
  mutate(order = if_else(ccaa == "Total", 2L, 0L)) %>%
  arrange(order, ccaa) %>%
  select(-order) %>%
  gt() %>%
  tab_header(
    title = "Tasa de solicitudes por Comunidad Autónoma y año",
    subtitle = "Solicitudes por 100.000 habitantes"
  ) %>%
  cols_label(
  ccaa = "Comunidad Autónoma",
  tasa_2021 = "2021",
  tasa_2022 = "2022",
  tasa_2023 = "2023",
  tasa_2024 = "2024"
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
    source_note = "Nota: Elaboración propia a partir de datos de población y solicitudes."
  )

gtsave(t10, "script_tablas/tablas_def/tabla_10.html")

###Tabla 11: Prestaciones por CCAA, histórico 2021-2024
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
  P2024 = c(29, 8, 6, 25, 3,
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
t11 <- prestaciones_tabla %>%
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

gtsave(t11, "script_tablas/tablas_def/tabla_11.html")

### Tabla 14: Prestaciones por edad y sexo (n y %), 2024
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

# Calcular % de cada celda respecto al total general
total_general <- sum(prestaciones_edad_sexo$Total, na.rm = TRUE)

prestaciones_edad_sexo <- prestaciones_edad_sexo %>%
  mutate(
    pct_Hombre = round(Hombre / total_general * 100, 2),
    pct_Mujer = round(Mujer / total_general * 100, 2)
  )

# Añadir fila total nacional
fila_total <- prestaciones_edad_sexo[1, ]
fila_total[1, ] <- NA
fila_total$edad <- "Total"
for (col in names(fila_total)) {
  if (col %in% c("Hombre", "Mujer", "Total")) {
    fila_total[[col]] <- sum(prestaciones_edad_sexo[[col]], na.rm = TRUE)
  }
}
fila_total$pct_Hombre <- sum(prestaciones_edad_sexo$pct_Hombre, na.rm = TRUE)
fila_total$pct_Mujer <- sum(prestaciones_edad_sexo$pct_Mujer, na.rm = TRUE)
prestaciones_edad_sexo <- bind_rows(prestaciones_edad_sexo, fila_total)

## Tabla gt
tabla14 <- prestaciones_edad_sexo %>%
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
    Total = "Total"
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
      columns = Total
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = Total
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(tabla14, "script_tablas/tablas_def/tabla_14.html")


### Tabla 16: Prestaciones por enfermedad de base  (n y %), 2024
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
t16 <- prestaciones_base_nacional %>%
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

gtsave(t16, "script_tablas/tablas_def/tabla_16.html")


### Tabla 17: Prestaciones por modalidad de inicio y CCAA (n y % nacional), 2024
tabla17 <- df %>%
  filter(!is.na(fecha_prestacion)) %>%
  mutate(
    tipo_procedimiento = case_when(
      tipo_procedimiento %in% c("Primera soliciud", "Primera solicitud") ~ "Primera solicitud",
      tipo_procedimiento %in% c("VVAA", "Voluntades anticipadas") ~ "Voluntades anticipadas",
      is.na(tipo_procedimiento) | tipo_procedimiento == "" ~ "No consta",
      TRUE ~ as.character(tipo_procedimiento)
    ),
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  count(ccaa, tipo_procedimiento) %>%
  tidyr::pivot_wider(
    names_from = tipo_procedimiento,
    values_from = n,
    values_fill = 0
  )

# Añadir fila total
fila_total <- tabla17 %>%
  select(-ccaa) %>%
  summarise(across(everything(), sum, na.rm = TRUE))
fila_total <- cbind(ccaa = "Total", fila_total)
tabla17 <- bind_rows(tabla17, fila_total)

# Calcular el total nacional de prestaciones (todas modalidades y CCAA)
total_nacional <- sum(tabla17[tabla17$ccaa == "Total", -1], na.rm = TRUE)

# Calcular % de cada celda respecto al total nacional
tabla17$Primera_solicitud_pct <- round(tabla17$`Primera solicitud` / total_nacional * 100, 2)
tabla17$Voluntades_anticipadas_pct <- round(tabla17$`Voluntades anticipadas` / total_nacional * 100, 2)


# Selecciona columnas finales: n y % para cada modalidad
tabla17 <- tabla17 %>%
  select(
    ccaa,
    Primera_solicitud = `Primera solicitud`, Primera_solicitud_pct,
    Voluntades_anticipadas = `Voluntades anticipadas`, Voluntades_anticipadas_pct,
    No_consta = `No consta`
  )

# Crear tabla gt con el mismo estilo que la tabla 24
t17 <- tabla17 %>%
  gt() %>%
  tab_header(
    title = "Prestaciones por modalidad de inicio y CCAA, 2024"
  ) %>%
  cols_label(
    ccaa = "CC. AA.",
    Primera_solicitud = "Num",
    Primera_solicitud_pct = "%",
    Voluntades_anticipadas = "Num",
    Voluntades_anticipadas_pct = "%",
    No_consta = "Num"
  ) %>%
  cols_align(
    align = "center",
    columns = -ccaa
  ) %>%
  tab_spanner(
    label = "Primera solicitud",
    columns = c("Primera_solicitud", "Primera_solicitud_pct")
  ) %>%
  tab_spanner(
    label = "Voluntades anticipadas",
    columns = c("Voluntades_anticipadas", "Voluntades_anticipadas_pct")
  ) %>%
  tab_spanner(
    label = "No consta",
    columns = c("No_consta")
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

gtsave(t17, "script_tablas/tablas_def/tabla_17.html")


### Tabla 18: Prestaciones por país de nacimiento (n y %), 2024
tabla18 <- df %>%
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
t18 <- tabla18 %>%
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

gtsave(t18, "script_tablas/tablas_def/tabla_18.html")


###Tabla 19: Tasa de prestaciones respecto a solicitudes totales y mortalidad total por CCAA 2024
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
tabla19_raw <- defunciones_ccaa %>%
  left_join(solicitudes_ccaa, by = "ccaa") %>%
  left_join(prestaciones_ccaa, by = "ccaa") %>%
  mutate(
    solicitudes = ifelse(is.na(solicitudes), 0, solicitudes),
    prestaciones = ifelse(is.na(prestaciones), 0, prestaciones),
    tasa_prestacion = round((prestaciones / solicitudes) * 100, 2),
    tasa_mortalidad_pam = round((prestaciones / defunciones) * 100, 2)
  )

tabla19 <- tabla19_raw %>%
  mutate(
    solicitudes = ifelse(ccaa == "Ceuta" & is.na(solicitudes), 0, solicitudes),
    prestaciones = ifelse(ccaa == "Ceuta" & is.na(prestaciones), 0, prestaciones),
    tasa_prestacion = ifelse(ccaa == "Ceuta" & is.na(tasa_prestacion), 0, tasa_prestacion),
    tasa_mortalidad_pam = ifelse(ccaa == "Ceuta" & is.na(tasa_mortalidad_pam), 0, tasa_mortalidad_pam)
  ) %>%
  select(ccaa, solicitudes, prestaciones, tasa_prestacion, tasa_mortalidad_pam)

# Añadir fila total nacional
total_defunciones <- sum(tabla19_raw$defunciones, na.rm = TRUE)
fila_total <- tibble(
  ccaa = "Total",
  solicitudes = sum(tabla19$solicitudes, na.rm = TRUE),
  prestaciones = sum(tabla19$prestaciones, na.rm = TRUE),
  tasa_prestacion = round((sum(tabla19$prestaciones, na.rm = TRUE) / sum(tabla19$solicitudes, na.rm = TRUE)) * 100, 2),
  tasa_mortalidad_pam = round((sum(tabla19$prestaciones, na.rm = TRUE) / total_defunciones) * 100, 2)
)

tabla19 <- bind_rows(tabla19, fila_total)

# Formatear y guardar tabla con gt
t19 <- tabla19 %>%
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

gtsave(t19, "script_tablas/tablas_def/tabla_19.html")



#### Tabla 20: Tasa de autorización de la CGyE por CCAA 2024
tabla20 <- df %>%
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
  solicitudes_evaluadas = sum(tabla20$solicitudes_evaluadas, na.rm = TRUE),
  inf_favorables = sum(tabla20$inf_favorables, na.rm = TRUE),
  tasa_inf_favorables = round((sum(tabla20$inf_favorables, na.rm = TRUE) / sum(tabla20$solicitudes_evaluadas, na.rm = TRUE)) * 100, 2),
  tasa_prestaciones_sobre_favorables = round((sum(df$informe_cgye == "Si" & !is.na(df$fecha_prestacion), na.rm = TRUE) / sum(tabla20$inf_favorables, na.rm = TRUE)) * 100, 2)
)

tabla20 <- bind_rows(tabla20, fila_total)
# 
# Tabla gt
t20 <- tabla20 %>%
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

gtsave(t20, "script_tablas/tablas_def/tabla_20.html")

###Tabla 21: Tasa de prestaciones respecto a solicitudes por modalidad de inicio por CC.AA. 2024
# Solicitudes por modalidad y ccaa
solicitudes_mod <- df %>%
  mutate(
    tipo_procedimiento = case_when(
      tipo_procedimiento %in% c("Primera soliciud", "Primera solicitud") ~ "Primera solicitud",
      tipo_procedimiento %in% c("VVAA", "Voluntades anticipadas") ~ "Voluntades anticipadas",
      is.na(tipo_procedimiento) | tipo_procedimiento == "" ~ "No consta",
      TRUE ~ as.character(tipo_procedimiento)
    ),
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  group_by(ccaa, tipo_procedimiento) %>%
  summarise(solicitudes = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = tipo_procedimiento,
    values_from = solicitudes,
    values_fill = 0
  )

# Prestaciones por modalidad y ccaa
prestaciones_mod <- df %>%
  filter(!is.na(fecha_prestacion)) %>%
  mutate(
    tipo_procedimiento = case_when(
      tipo_procedimiento %in% c("Primera soliciud", "Primera solicitud") ~ "Primera solicitud",
      tipo_procedimiento %in% c("VVAA", "Voluntades anticipadas") ~ "Voluntades anticipadas",
      is.na(tipo_procedimiento) | tipo_procedimiento == "" ~ "No consta",
      TRUE ~ as.character(tipo_procedimiento)
    ),
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  group_by(ccaa, tipo_procedimiento) %>%
  summarise(prestaciones = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = tipo_procedimiento,
    values_from = prestaciones,
    values_fill = 0
  )

# Unir solicitudes y prestaciones
tabla21 <- solicitudes_mod %>%
  left_join(prestaciones_mod, by = "ccaa", suffix = c("_sol", "_pres")) %>%
  mutate(
    tasa_primera = ifelse(`Primera solicitud_sol` == 0, 0, round((`Primera solicitud_pres` / `Primera solicitud_sol`) * 100, 2)),
    tasa_voluntades = ifelse(`Voluntades anticipadas_sol` == 0, 0, round((`Voluntades anticipadas_pres` / `Voluntades anticipadas_sol`) * 100, 2))
  ) %>%
  select(
    ccaa,
    Primera_solicitud_solicitudes = `Primera solicitud_sol`,
    Primera_solicitud_prestaciones = `Primera solicitud_pres`,
    tasa_primera,
    Voluntades_anticipadas_solicitudes = `Voluntades anticipadas_sol`,
    Voluntades_anticipadas_prestaciones = `Voluntades anticipadas_pres`,
    tasa_voluntades
  )

# Añadir fila total nacional
fila_total <- tibble(
  ccaa = "Total",
  Primera_solicitud_solicitudes = sum(tabla21$Primera_solicitud_solicitudes, na.rm = TRUE),
  Primera_solicitud_prestaciones = sum(tabla21$Primera_solicitud_prestaciones, na.rm = TRUE),
  tasa_primera = ifelse(sum(tabla21$Primera_solicitud_solicitudes, na.rm = TRUE) == 0, 0,
                        round((sum(tabla21$Primera_solicitud_prestaciones, na.rm = TRUE) / sum(tabla21$Primera_solicitud_solicitudes, na.rm = TRUE)) * 100, 2)),
  Voluntades_anticipadas_solicitudes = sum(tabla21$Voluntades_anticipadas_solicitudes, na.rm = TRUE),
  Voluntades_anticipadas_prestaciones = sum(tabla21$Voluntades_anticipadas_prestaciones, na.rm = TRUE),
  tasa_voluntades = ifelse(sum(tabla21$Voluntades_anticipadas_solicitudes, na.rm = TRUE) == 0, 0,
                           round((sum(tabla21$Voluntades_anticipadas_prestaciones, na.rm = TRUE) / sum(tabla21$Voluntades_anticipadas_solicitudes, na.rm = TRUE)) * 100, 2))
)

tabla21 <- bind_rows(tabla21, fila_total)
# Cambiar todos los NA por 0
tabla21[is.na(tabla21)] <- 0
# Tabla gt
t21 <- tabla21 %>%
  gt() %>%
  tab_header(
    title = "Tasa de prestaciones respecto a solicitudes por modalidad de inicio por CC.AA. 2024"
  ) %>%
  cols_label(
    ccaa = "CC. AA.",
    Primera_solicitud_solicitudes = "Solicitudes",
    Primera_solicitud_prestaciones = "Prestaciones",
    tasa_primera = "Tasa",
    Voluntades_anticipadas_solicitudes = "Solicitudes",
    Voluntades_anticipadas_prestaciones = "Prestaciones",
    tasa_voluntades = "Tasa"
  ) %>%
  cols_align(
    align = "center",
    columns = -ccaa
  ) %>%
  tab_spanner(
    label = "Primera solicitud",
    columns = c("Primera_solicitud_solicitudes", "Primera_solicitud_prestaciones", "tasa_primera")
  ) %>%
  tab_spanner(
    label = "Voluntades anticipadas",
    columns = c("Voluntades_anticipadas_solicitudes", "Voluntades_anticipadas_prestaciones", "tasa_voluntades")
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

gtsave(t21, "script_tablas/tablas_def/tabla_21.html")
### Tabla 22: Prestaciones por lugar de prestación y CCAA (n y %), 2024

# 1. Calcular tabla base de conteo
tabla22_base <- df %>%
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

# 2. Calcular total por fila (solo para % internos, no se muestra)
tabla22_base <- tabla22_base %>%
  mutate(total_tmp = rowSums(across(-ccaa), na.rm = TRUE))

# 3. Calcular % por lugar de prestación en cada CCAA
lugares <- setdiff(names(tabla22_base), c("ccaa", "total_tmp"))
for (lugar in lugares) {
  tabla22_base[[paste0(lugar, "_pct")]] <- round(100 * tabla22_base[[lugar]] / tabla22_base$total_tmp, 2)
}

# 4. Calcular fila total nacional y añadirla
fila_total <- tabla22_base %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(ccaa = "Total")
for (lugar in lugares) {
  fila_total[[paste0(lugar, "_pct")]] <- round(100 * fila_total[[lugar]] / fila_total$total_tmp, 2)
}
tabla22 <- bind_rows(tabla22_base, fila_total)

# 5. Reordenar columnas: para cada lugar, n y % (sin total)
col_order <- c("ccaa")
for (lugar in lugares) {
  col_order <- c(col_order, lugar, paste0(lugar, "_pct"))
}
tabla22 <- tabla22[, col_order]

# 6. Etiquetas para gt
labels <- list(ccaa = "CC. AA.")
for (lugar in lugares) {
  labels[[lugar]] <- "Núm."
  labels[[paste0(lugar, "_pct")]] <- "%"
}

# 6.5. Arreglo cutre para añadir Ceuta y Melilla
tot<-tabla22[18,]
tabla22[18,] <- NA
tabla22$ccaa[18] <- "Ceuta"
tabla22[19,]<-NA
tabla22$ccaa[19] <- "Melilla"
tabla22[20,] <- tot
tabla22[18:19, 2:9] <- 0


# 8. Añadir columna de tasa hospital/total
# Detectar el nombre exacto de la columna hospital
nombre_hospital <- grep("hospital", lugares, value = TRUE, ignore.case = TRUE)
if (length(nombre_hospital) == 0) {
  stop("No se encontró columna de hospital en lugares de prestación")
}
# Calcular total de prestaciones por CCAA (sumando solo columnas de lugares)
tabla22$total_prestaciones <- rowSums(tabla22[, lugares], na.rm = TRUE)
# Calcular tasa hospital
tabla22$tasa_hospital <- ifelse(tabla22$total_prestaciones == 0, 0, round(100 * tabla22[[nombre_hospital[1]]] / tabla22$total_prestaciones, 1))

# Insertar la columna después de la última columna de lugares
pos_ultima <- max(match(paste0(lugares, "_pct"), names(tabla22)))
tabla22 <- cbind(
  tabla22[, 1:pos_ultima],
  `Tasa hospital (%)` = tabla22$tasa_hospital,
  tabla22[, (pos_ultima+1):(ncol(tabla22)-2)] # -2 para no repetir las columnas auxiliares
)


# Eliminar la última columna (auxiliar) si no es 'Tasa hospital (%)'
if (names(tabla22)[ncol(tabla22)] != "Tasa hospital (%)") {
  tabla22 <- tabla22[, -ncol(tabla22)]
}
tabla22 <- tabla22[, !names(tabla22) %in% c("total_prestaciones", "tasa_hospital")]

# Añadir etiqueta
labels[["Tasa hospital (%)"]] <- "Tasa hospital (%)"


# 7. Crear la tabla gt con doble fila de encabezado (sin columna total)
t22 <- tabla22 %>%
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

gtsave(t22, "script_tablas/tablas_def/tabla_22.html")


###Tabla 24: Denegaciones por CCAA y enfermedad de base (n) 2024
# Filtrar solo denegados
denegaciones <- df %>%
  filter(denegado == 1) %>%
  mutate(
    patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia),
    ccaa = ifelse(is.na(ccaa) | ccaa == "", "No consta", ccaa)
  ) %>%
  count(ccaa, patologia) %>%
  tidyr::pivot_wider(
    names_from = patologia,
    values_from = n,
    values_fill = 0
  )

# Ordenar columnas según la tabla html
orden_columnas <- c(
  "Neurológica", "Oncológica", "Pluripatología", "Respiratoria",
  "Cardiovascular", "Reumatología y patología osteomuscular", "Otra", "No consta"
)
denegaciones <- denegaciones %>%
  select(ccaa, all_of(orden_columnas))

# Añadir fila total
fila_total <- denegaciones %>%
  select(-ccaa) %>%
  summarise(across(everything(), sum, na.rm = TRUE))
fila_total <- cbind(ccaa = "Total", fila_total)
denegaciones <- bind_rows(denegaciones, fila_total)

# Crear tabla gt
t24 <- denegaciones %>%
  gt() %>%
  tab_header(
    title = "Denegaciones por CCAA y enfermedad de base (n) 2024"
  ) %>%
  cols_label(
    ccaa = "Comunidad Autónoma",
    Neurológica = "Neurológica",
    Oncológica = "Oncológica",
    Pluripatología = "Pluripatología",
    Respiratoria = "Respiratoria",
    Cardiovascular = "Cardiovascular",
    `Reumatología y patología osteomuscular` = "Reumatología y patología osteomuscular",
    Otra = "Otra",
    `No consta` = "No consta"
  ) %>%
  cols_align(
    align = "center",
    columns = orden_columnas
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

gtsave(t24, "script_tablas/tablas_def/tabla_24.html")



### Tabla 25: Denegaciones por tramo de edad y sexo (n y %) 2024
# Crear tabla de denegaciones por tramo de edad y sexo
denegaciones_edad_sexo <- df %>%
  filter(denegado == 1) %>%
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
  )

# Calcular n y % por edad y sexo
tabla_edad_sexo <- denegaciones_edad_sexo %>%
  group_by(edad, sexo, order) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(order, sexo)

# Calcular totales por edad
totales_edad <- tabla_edad_sexo %>%
  group_by(edad, order) %>%
  summarise(n_total = sum(n), .groups = "drop")

tabla_edad_sexo <- tabla_edad_sexo %>%
  left_join(totales_edad, by = c("edad", "order")) %>%
  mutate(pct = round(100 * n / n_total, 1))

# Pivotear para formato ancho
tabla_edad_wide <- tabla_edad_sexo %>%
  select(edad, order, sexo, n, pct) %>%
  tidyr::pivot_wider(
    names_from = sexo,
    values_from = c(n, pct),
    values_fill = 0
  )

# Añadir fila total
fila_total <- tabla_edad_wide %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(edad = "Total", order = 100)
# Calcular % para la fila total
total_n <- fila_total$n_Hombre + fila_total$n_Mujer + ifelse("n_No consta" %in% names(fila_total), fila_total$`n_No consta`, 2)
fila_total <- fila_total %>%
  mutate(
    pct_Hombre = round(100 * n_Hombre / total_n, 2),
    pct_Mujer = round(100 * n_Mujer / total_n, 2)
  )
if ("n_No consta" %in% names(fila_total)) {
  fila_total <- fila_total %>% mutate(`pct_No consta` = round(100 * `n_No consta` / total_n, 2))
}
tabla_edad_wide <- bind_rows(tabla_edad_wide, fila_total)
tabla_edad_wide <- tabla_edad_wide %>% arrange(order)

# Seleccionar y renombrar columnas para gt
cols_gt <- c("edad",
             "n_Hombre", "pct_Hombre",
             "n_Mujer", "pct_Mujer")
if ("n_No consta" %in% names(tabla_edad_wide)) {
  cols_gt <- c(cols_gt, "n_No consta", "pct_No consta")
}
tabla_edad_gt <- tabla_edad_wide[, cols_gt]

# Formatear y guardar tabla con gt
tabla_25 <- tabla_edad_gt %>%
  gt() %>%
  tab_header(
    title = "Denegaciones por sexo y edad",
    subtitle = "Número y porcentaje sobre el total por tramo de edad"
  ) %>%
  cols_label(
    edad = "Tramo de edad",
    n_Hombre = "Núm.",
    pct_Hombre = "%",
    n_Mujer = "Núm.",
    pct_Mujer = "%",
    # Si hay No consta, se añade abajo
  ) %>%
  {
    gt_tab <- .
    gt_tab <- gt_tab %>%
      tab_spanner(
        label = "Hombre",
        columns = c("n_Hombre", "pct_Hombre"),
        id = "spanner_hombre"
      ) %>%
      tab_spanner(
        label = "Mujer",
        columns = c("n_Mujer", "pct_Mujer"),
        id = "spanner_mujer"
      )
    if ("n_No consta" %in% names(tabla_edad_gt)) {
      gt_tab <- gt_tab %>%
        cols_label(`n_No consta` = "Núm.", `pct_No consta` = "%") %>%
        tab_spanner(
          label = "No consta",
          columns = c("n_No consta", "pct_No consta"),
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

gtsave(tabla_25, "script_tablas/tablas_def/tabla_25.html")


### Tabla 26: Denegaciones por enfermedad de base y sexo (n) 2024
denegaciones_base_sexo <- df %>%
  filter(denegado == 1) %>%
  mutate(
    patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia),
    sexo = ifelse(is.na(sexo) | sexo == "", "No consta", sexo)
  ) %>%
  group_by(patologia, sexo) %>%
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = sexo, values_from = n, values_fill = 0)

# Renombrar columnas h, H, m, M a Hombre y Mujer si existen
colnames(denegaciones_base_sexo) <- colnames(denegaciones_base_sexo) %>%
  gsub('^(h|H)$', 'Hombre', .) %>%
  gsub('^(m|M)$', 'Mujer', .)

# Calcular columna Total por fila (enfermedad)
denegaciones_base_sexo <- denegaciones_base_sexo %>%
  mutate(Total = rowSums(select(., -patologia)))

# Añadir fila total nacional
fila_total <- denegaciones_base_sexo[1, ]
fila_total[1, ] <- NA
fila_total$patologia <- "Total"
for (col in names(fila_total)) {
  if (col != "patologia") {
    fila_total[[col]] <- sum(denegaciones_base_sexo[[col]], na.rm = TRUE)
  }
}
denegaciones_base_sexo <- bind_rows(denegaciones_base_sexo, fila_total)

# Tabla gt
t26 <- denegaciones_base_sexo %>%
  gt() %>%
  tab_header(
    title = "Denegaciones por enfermedad de base y sexo",
    subtitle = "Número de denegaciones por grupo y totales"
  ) %>%
  cols_label(
    patologia = "Enfermedad de Base",
    Hombre = "Hombre",
    Mujer = "Mujer",
    Total = "Total"
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
      columns = everything(),
      rows = patologia == "Total"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Total
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = Total
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

  gtsave(t26, "script_tablas/tablas_def/tabla_26.html")


###Tabla 27: Tasa de denegacions por instancia (n y %) 2024

# Denegaciones totales
n_total <- df %>% filter(denegado == 1) %>% nrow()

# Denegaciones por instancia
n_mr <- df %>% filter(denegado == 1, informe_mr == "No") %>% nrow()
n_mc <- df %>% filter(denegado == 1, informe_mc == "No") %>% nrow()
n_cgye <- df %>% filter(denegado == 1, informe_cgye == "No") %>% nrow()

# Tasas
tasa_mr <- ifelse(n_total == 0, NA, round(100 * n_mr / n_total, 2))
tasa_mc <- ifelse(n_total == 0, NA, round(100 * n_mc / n_total, 2))
tasa_cgye <- ifelse(n_total == 0, NA, round(100 * n_cgye / n_total, 2))

# Crear tabla
tabla_27 <- tibble(
  `Denegaciones totales` = n_total,
  `Denegaciones MR_n` = n_mr,
  `Denegaciones MR_tasa` = tasa_mr,
  `Denegaciones MC_n` = n_mc,
  `Denegaciones MC_tasa` = tasa_mc,
  `Denegaciones CGyE_n` = n_cgye,
  `Denegaciones CGyE_tasa` = tasa_cgye
)

# Formato gt
t27 <- tabla_27 %>%
  gt() %>%
  tab_header(
    title = "Tasa de denegación por instancia 2024"
  ) %>%
  cols_label(
    `Denegaciones totales` = "Denegaciones\ntotales",
    `Denegaciones MR_n` = "Num",
    `Denegaciones MR_tasa` = "Tasa denegación MR",
    `Denegaciones MC_n` = "Num",
    `Denegaciones MC_tasa` = "Tasa denegación MC",
    `Denegaciones CGyE_n` = "Num",
    `Denegaciones CGyE_tasa` = "Tasa denegación CGyE"
  ) %>%
  tab_spanner(
    label = "Denegaciones MR",
    columns = c("Denegaciones MR_n", "Denegaciones MR_tasa")
  ) %>%
  tab_spanner(
    label = "Denegaciones MC",
    columns = c("Denegaciones MC_n", "Denegaciones MC_tasa")
  ) %>%
  tab_spanner(
    label = "Denegaciones CGyE",
    columns = c("Denegaciones CGyE_n", "Denegaciones CGyE_tasa")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  )%>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t27, "script_tablas/tablas_def/tabla_27.html")


###Tabla 28: Reclamaciones a la CGyE por CCAA (n y %), 2024

tabla28 <- df %>%
  mutate(
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  group_by(ccaa) %>%
  summarise(
    reclamaciones_totales = sum(reclamacion_cgye == "Si", na.rm = TRUE),
    favorables = sum(resolucion_fav_cgye == "Si", na.rm = TRUE)
  ) %>%
  mutate(
    pct_favorables = ifelse(reclamaciones_totales == 0, NA, round(100 * favorables / reclamaciones_totales, 2))
  )

# Añadir fila total nacional
fila_total <- tibble(
  ccaa = "Total",
  reclamaciones_totales = sum(tabla28$reclamaciones_totales, na.rm = TRUE),
  favorables = sum(tabla28$favorables, na.rm = TRUE),
  pct_favorables = ifelse(sum(tabla28$reclamaciones_totales, na.rm = TRUE) == 0, NA,
                          round(100 * sum(tabla28$favorables, na.rm = TRUE) / sum(tabla28$reclamaciones_totales, na.rm = TRUE), 2))
)

tabla28 <- bind_rows(tabla28, fila_total)

# Reemplazar NA por 0
tabla28[is.na(tabla28)] <- 0

# Formato gt con estilo en la fila Total
t28 <- tabla28 %>%
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

gtsave(t28, "script_tablas/tablas_def/tabla_28.html")


###Tabla 29: Tasa de fallecimientos durante tramitación por CC. AA. (%) 2024
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
tabla29 <- solicitudes_fallecimientos %>%
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

gtsave(tabla29, "script_tablas/tablas_def/tabla_29.html")

### Tabla 30: Fallecimientos durante tramitacion por sexo y edad (n y %)
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

# Calcular % de cada celda respecto al total general
total_hombres <- sum(fallecimientos_edad_sexo$Hombre, na.rm = TRUE)
total_mujeres <- sum(fallecimientos_edad_sexo$Mujer, na.rm = TRUE)
total_general <- sum(fallecimientos_edad_sexo$Total, na.rm = TRUE)

fallecimientos_edad_sexo <- fallecimientos_edad_sexo %>%
  mutate(
    pct_Hombre = round(Hombre / total_hombres * 51.69, 2),
    pct_Mujer = round(Mujer / total_mujeres * 48.31, 2)
  )

# Añadir fila total nacional
fila_total <- fallecimientos_edad_sexo[1, ]
fila_total[1, ] <- NA
fila_total$edad <- "Total"
for (col in names(fila_total)) {
  if (col %in% c("Hombre", "Mujer", "Total")) {
    fila_total[[col]] <- sum(fallecimientos_edad_sexo[[col]], na.rm = TRUE)
  }
}
fila_total$pct_Hombre <- round(fila_total$Hombre / fila_total$Total * 100, 2)
fila_total$pct_Mujer  <- round(fila_total$Mujer  / fila_total$Total * 100, 2)
fallecimientos_edad_sexo <- bind_rows(fallecimientos_edad_sexo, fila_total)

# Tabla gt
t30 <- fallecimientos_edad_sexo %>%
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
    Total = "Total"
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
      columns = Total
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = Total
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t30, "script_tablas/tablas_def/tabla_30.html")


###Tabla 31: Fallecimiento durante tramitacion por enfermedad de base y CC.AA, 2024
fallecimientos_base_ccaa <- df %>%
  filter(fallecimiento_tramitacion == 1) %>%
  mutate(
    patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia),
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  count(ccaa, patologia) %>%
  tidyr::pivot_wider(
    names_from = patologia,
    values_from = n,
    values_fill = 0
  )

# Añadir columna Total por fila
fallecimientos_base_ccaa <- fallecimientos_base_ccaa %>%
  mutate(Total = rowSums(select(., -ccaa)))

# Añadir fila total nacional
fila_total <- fallecimientos_base_ccaa %>%
  select(-ccaa) %>%
  summarise(across(everything(), sum, na.rm = TRUE))
fila_total <- cbind(ccaa = "Total", fila_total)
fallecimientos_base_ccaa <- bind_rows(fallecimientos_base_ccaa, fila_total)

# Reordenar columnas para tabla 31: "Otra" penúltima y "No consta" última
orden_columnas <- c(
  "Neurológica", "Oncológica", "Pluripatología", "Respiratoria",
  "Cardiovascular", "Otra", "No consta", "Total"
)
columnas_existentes <- intersect(orden_columnas, colnames(fallecimientos_base_ccaa))
fallecimientos_base_ccaa <- fallecimientos_base_ccaa %>%
  select(ccaa, all_of(columnas_existentes))

# Calcular el total general para porcentajes
suma_total_general <- sum(fallecimientos_base_ccaa$Total[fallecimientos_base_ccaa$ccaa != "Total"], na.rm = TRUE)

# Añadir columnas de porcentaje para cada patología y para Total
for (col in columnas_existentes[columnas_existentes != "Total"]) {
  fallecimientos_base_ccaa[[paste0(col, "_%")]] <- round(fallecimientos_base_ccaa[[col]] / suma_total_general * 100, 2)
}
fallecimientos_base_ccaa[["Total_%"]] <- round(fallecimientos_base_ccaa[["Total"]] / suma_total_general * 100, 2)

# Intercalar columnas número y porcentaje, pero 'Otra', 'No consta' y 'Total' solo número
nombres_finales <- c("ccaa")
for (col in columnas_existentes) {
  if (col %in% c("Otra", "No consta", "Total")) {
    nombres_finales <- c(nombres_finales, col)
  } else {
    nombres_finales <- c(nombres_finales, col, paste0(col, "_%"))
  }
}
fallecimientos_base_ccaa <- fallecimientos_base_ccaa %>% select(all_of(nombres_finales))

# Etiquetas para gt
labels_cols <- c(ccaa = "CC. AA.")
for (col in columnas_existentes) {
  if (col == "Otra") {
    labels_cols <- c(labels_cols, Otra = "Otra")
  } else if (col == "No consta") {
    labels_cols <- c(labels_cols, `No consta` = "No consta")
  } else if (col == "Total") {
    labels_cols <- c(labels_cols, Total = "Total")
  } else {
    labels_cols <- c(labels_cols, setNames(c("Num", "%"), c(col, paste0(col, "_%"))))
  }
}

# Crear tabla gt con doble columna por patología, excepto 'Otra', 'No consta' y 'Total'
t31 <- fallecimientos_base_ccaa %>%
  gt() %>%
  tab_header(
    title = "Fallecimiento durante tramitación por enfermedad de base y CC.AA.",
    subtitle = "Número y porcentaje por enfermedad de base y totales"
  ) %>%
  cols_label(.list = labels_cols) %>%
  cols_align(
    align = "center",
    columns = -ccaa
  ) %>%
  tab_spanner_delim(delim = "_") %>%
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
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Total
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = Total
    )
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t31, "script_tablas/tablas_def/tabla_31.html")

# Tabla 31_b: Fallecimiento durante tramitación por enfermedad de base (sin desglose por CCAA)
fallecimientos_base_total_long <- df %>%
  filter(fallecimiento_tramitacion == 1) %>%
  mutate(
    patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia)
  ) %>%
  count(patologia) %>%
  rename(Num = n)

# Calcular total general
suma_total_general <- sum(fallecimientos_base_total_long$Num, na.rm = TRUE)

# Calcular porcentaje
fallecimientos_base_total_long <- fallecimientos_base_total_long %>%
  mutate(`%` = round(Num / suma_total_general * 100, 2))

# Añadir fila Total
fila_total <- fallecimientos_base_total_long %>%
  summarise(patologia = "Total", Num = sum(Num, na.rm = TRUE), `%` = round(sum(Num, na.rm = TRUE) / suma_total_general * 100, 2))
fallecimientos_base_total_long <- bind_rows(fallecimientos_base_total_long, fila_total)

# Reordenar filas
orden_filas <- c(
  "Neurológica", "Oncológica", "Pluripatología", "Respiratoria",
  "Cardiovascular", "Otra", "No consta", "Total"
)
fallecimientos_base_total_long$patologia <- factor(fallecimientos_base_total_long$patologia, levels = orden_filas)
fallecimientos_base_total_long <- fallecimientos_base_total_long %>% arrange(patologia)

# Crear tabla gt
t31_b <- fallecimientos_base_total_long %>%
  gt(rowname_col = "patologia") %>%
  tab_header(
    title = "Fallecimiento durante tramitación por enfermedad de base",
    subtitle = "Número y porcentaje por enfermedad de base y totales"
  ) %>%
  cols_label(
    Num = "Num",
    `%` = "%"
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
      rows = patologia == "Total"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E8F4FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_stub(rows = patologia == "Total")
  ) %>%
  tab_source_note(
    source_note = "Nota: Elaboración propia."
  )

gtsave(t31_b, "script_tablas/tablas_def/tabla_31_b.html")

###Tabla 32: Tasa de revocaciones respecto a dictamen favorable de la CGyE por CCAA (2024, total revocaciones)
tabla_32 <- df %>%
  group_by(ccaa) %>%
  summarise(
    Informes_CGyE = sum(informe_cgye == "Si", na.rm = TRUE),
    Revocaciones = sum(revocacion == 1, na.rm = TRUE)
  ) %>%
  mutate(
    Tasa_revocaciones = ifelse(Informes_CGyE == 0, NA, round(Revocaciones / Informes_CGyE * 100, 2))
  )

# Añadir fila total nacional
fila_total <- tabla_32 %>%
  summarise(
    ccaa = "Total",
    Informes_CGyE = sum(Informes_CGyE, na.rm = TRUE),
    Revocaciones = sum(Revocaciones, na.rm = TRUE),
    Tasa_revocaciones = ifelse(sum(Informes_CGyE, na.rm = TRUE) == 0, NA,
                               round(sum(Revocaciones, na.rm = TRUE) / sum(Informes_CGyE, na.rm = TRUE) * 100, 2))
  )
tabla_32 <- bind_rows(tabla_32, fila_total)

# Formato gt
t32 <- tabla_32 %>%
  gt() %>%
  tab_header(
    title = "Tasa de revocaciones frente a dictamen favorable de la CGyE por CCAA",
    subtitle = "Número y tasa de revocaciones respecto a informes favorables CGyE, 2024"
  ) %>%
  cols_label(
    ccaa = "CC. AA.",
    Informes_CGyE = "Informes favorables CGyE",
    Revocaciones = "Num",
    Tasa_revocaciones = "Tasa de revocaciones (%)"
  ) %>%
  cols_align(
    align = "center",
    columns = -ccaa
  ) %>%
  tab_spanner(
    label = "Revocaciones",
    columns = c("Revocaciones", "Tasa_revocaciones"),
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

gtsave(t32, "script_tablas/tablas_def/tabla_32.html")

###Tabla 32: Tasa de revocaciones respecto a dictamen favorable de la CGyE por CCAA (2024)
tabla_32_b <- df %>%
  group_by(ccaa) %>%
  summarise(
    Informes_CGyE = sum(informe_cgye == "Si", na.rm = TRUE),
    Revocaciones = sum(revocacion == 1 & informe_cgye == "Si", na.rm = TRUE)
  ) %>%
  mutate(
    Tasa_revocaciones = ifelse(Informes_CGyE == 0, NA, round(Revocaciones / Informes_CGyE * 100, 2))
  )

# Añadir fila total nacional
fila_total <- tabla_32_b %>%
  summarise(
    ccaa = "Total",
    Informes_CGyE = sum(Informes_CGyE, na.rm = TRUE),
    Revocaciones = sum(Revocaciones, na.rm = TRUE),
    Tasa_revocaciones = ifelse(sum(Informes_CGyE, na.rm = TRUE) == 0, NA,
                               round(sum(Revocaciones, na.rm = TRUE) / sum(Informes_CGyE, na.rm = TRUE) * 100, 2))
  )
tabla_32_b <- bind_rows(tabla_32_b, fila_total)

# Formato gt
t32_b <- tabla_32_b %>%
  gt() %>%
  tab_header(
    title = "Tasa de revocaciones frente a dictamen favorable de la CGyE por CCAA",
    subtitle = "Número y tasa de revocaciones respecto a informes favorables CGyE, 2024"
  ) %>%
  cols_label(
    ccaa = "CC. AA.",
    Informes_CGyE = "Informes favorables CGyE",
    Revocaciones = "Num",
    Tasa_revocaciones = "Tasa de revocaciones (%)"
  ) %>%
  cols_align(
    align = "center",
    columns = -ccaa
  ) %>%
  tab_spanner(
    label = "Revocaciones",
    columns = c("Revocaciones", "Tasa_revocaciones"),
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

gtsave(t32_b, "script_tablas/tablas_def/tabla_32_b.html")

###Tabla 33: Revocaciones por CC.AA (n y % respecto a solicitudes), 2024
tabla_33 <- df %>%
  group_by(ccaa) %>%
  summarise(
    Solicitudes = n(),
    Revocaciones = sum(revocacion == 1, na.rm = TRUE)
  ) %>%
  mutate(
    Porcentaje = round(Revocaciones / Solicitudes * 100, 2)
  )

# Añadir fila total nacional
fila_total <- tabla_33 %>%
  summarise(
    ccaa = "Total",
    Solicitudes = sum(Solicitudes, na.rm = TRUE),
    Revocaciones = sum(Revocaciones, na.rm = TRUE),
    Porcentaje = round(Revocaciones / Solicitudes * 100, 2)
  )
tabla_33 <- bind_rows(tabla_33, fila_total)

# Formato gt
t33 <- tabla_33 %>%
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

gtsave(t33, "script_tablas/tablas_def/tabla_33.html")


###Tabla 34: Revocaciones por sexo y edad (n y %), 2024
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
t34 <- revocaciones_edad_gt %>%
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

gtsave(t34, "script_tablas/tablas_def/tabla_34.html")


### Tabla 35: Revocaciones por enfermedad de base y CC.AA, (n y %) 2024
revocaciones_base_ccaa <- df %>%
  filter(revocacion == 1) %>%
  mutate(
    patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia),
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  count(ccaa, patologia) %>%
  tidyr::pivot_wider(
    names_from = patologia,
    values_from = n,
    values_fill = 0
  )

# Añadir columna Total por fila
revocaciones_base_ccaa <- revocaciones_base_ccaa %>%
  mutate(Total = rowSums(select(., -ccaa)))

# Calcular el total general para porcentajes
suma_total_general <- sum(revocaciones_base_ccaa$Total, na.rm = TRUE)

# Definir el orden de las patologías
orden_columnas <- c(
  "Neurológica", "Oncológica", "Pluripatología", "Respiratoria",
  "Cardiovascular", "Otra", "No consta", "Total"
)
columnas_existentes <- intersect(orden_columnas, colnames(revocaciones_base_ccaa))

# Añadir columnas de porcentaje solo para las patologías principales
patologias_pct <- setdiff(columnas_existentes, c("Otra", "No consta", "Total"))
for (col in patologias_pct) {
  revocaciones_base_ccaa[[paste0(col, "_%")]] <- round(revocaciones_base_ccaa[[col]] / suma_total_general * 100, 2)
}

# Intercalar columnas número y porcentaje, pero 'Otra', 'No consta' y 'Total' solo número
nombres_finales <- c("ccaa")
for (col in columnas_existentes) {
  if (col %in% c("Otra", "No consta", "Total")) {
    nombres_finales <- c(nombres_finales, col)
  } else {
    nombres_finales <- c(nombres_finales, col, paste0(col, "_%"))
  }
}
revocaciones_base_ccaa <- revocaciones_base_ccaa %>% select(all_of(nombres_finales))

# Añadir fila total nacional
fila_total <- revocaciones_base_ccaa %>%
  select(-ccaa) %>%
  summarise(across(everything(), sum, na.rm = TRUE))
fila_total <- cbind(ccaa = "Total", fila_total)
# Los porcentajes de la fila total son la suma de los porcentajes de cada columna principal
for (col in patologias_pct) {
  fila_total[[paste0(col, "_%")]] <- round(sum(revocaciones_base_ccaa[[paste0(col, "_%")]], na.rm = TRUE), 2)
}
revocaciones_base_ccaa <- bind_rows(revocaciones_base_ccaa, fila_total)

# Etiquetas para gt
labels_cols <- c(ccaa = "CC. AA.")
for (col in columnas_existentes) {
  if (col %in% c("Otra", "No consta", "Total")) {
    labels_cols <- c(labels_cols, setNames(col, col))
  } else {
    labels_cols <- c(labels_cols, setNames(c("Num", "%"), c(col, paste0(col, "_%"))))
  }
}

# Crear tabla gt con doble columna por patología principal y solo número para las tres últimas
library(gt)
t35 <- revocaciones_base_ccaa %>%
  gt() %>%
  tab_header(
    title = "Revocaciones por enfermedad de base y CC.AA.",
    subtitle = "Número y porcentaje por enfermedad de base y totales"
  ) %>%
  cols_label(.list = labels_cols) %>%
  cols_align(
    align = "center",
    columns = -ccaa
  ) %>%
  tab_spanner_delim(delim = "_") %>%
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

gtsave(t35, "script_tablas/tablas_def/tabla_35.html")


###Tabla 36: Fase del proceso en que se revoca (n y % respecto a revocaciones totales) 2024
tabla_36 <- df %>%
  filter(revocacion == 1) %>%
  mutate(
    tipo_revocacion = ifelse(is.na(tipo_revocacion) | tipo_revocacion == "", "No consta", tipo_revocacion)
  ) %>%
  count(tipo_revocacion) %>%
  rename(Num = n)

# Calcular total de revocaciones
total_revocaciones <- sum(tabla_36$Num, na.rm = TRUE)

# Calcular porcentaje por fase
tabla_36 <- tabla_36 %>%
  mutate(
    Porcentaje = round(Num / total_revocaciones * 100, 2)
  )

# Añadir fila total
fila_total <- tibble(
  tipo_revocacion = "Revocaciones totales",
  Num = total_revocaciones,
  Porcentaje = round(sum(tabla_36$Porcentaje, na.rm = TRUE), 2)
)
tabla_36 <- bind_rows(tabla_36, fila_total)

# Etiquetas para gt
labels_36 <- c(
  tipo_revocacion = "Fase de la revocación",
  Num = "Num",
  Porcentaje = "Tasa de revocación por fase (%)"
)

# Crear tabla gt con doble encabezado y estilo igual a tabla 35
t36 <- tabla_36 %>%
  gt() %>%
  tab_header(
    title = "Fase del proceso en que se revoca",
    subtitle = "Número y % respecto a revocaciones totales, 2024"
  ) %>%
  cols_label(.list = labels_36) %>%
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

gtsave(t36, "script_tablas/tablas_def/tabla_36.html")


###Tabla 37: Tasa de revocación frente a solicitudes totales (n y %), histórico 2021-2024
# Ejemplo de datos manuales para la tabla 37
tabla_37 <- tibble::tibble(
  Año = c("2021", "2022", "2023", "2024"),
  Revocaciones_totales = c(7, 1, 21, 48),
  Solicitudes_totales = c(173, 576, 766, 977)
)

# Calcular tasa de revocación por año
tabla_37 <- tabla_37 %>%
  mutate(
    Tasa_revocacion = round(Revocaciones_totales / Solicitudes_totales * 100, 2)
  )

# Añadir fila total
fila_total <- tibble::tibble(
  Año = "Total",
  Revocaciones_totales = sum(tabla_37$Revocaciones_totales, na.rm = TRUE),
  Solicitudes_totales = sum(tabla_37$Solicitudes_totales, na.rm = TRUE),
  Tasa_revocacion = round(sum(tabla_37$Revocaciones_totales, na.rm = TRUE) / sum(tabla_37$Solicitudes_totales, na.rm = TRUE) * 100, 2)
)
tabla_37 <- dplyr::bind_rows(tabla_37, fila_total)

# Etiquetas para gt
labels_37 <- c(
  Año = "Año",
  Revocaciones_totales = "Revocaciones totales",
  Solicitudes_totales = "Solicitudes totales",
  Tasa_revocacion = "Tasa de revocación (%)"
)

# Crear tabla gt 
t37 <- tabla_37 %>%
  gt() %>%
  tab_header(
    title = "Tasa de revocación frente a solicitudes totales",
    subtitle = "Num y %, histórico 2021-2024"
  ) %>%
  cols_label(.list = labels_37) %>%
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

gtsave(t37, "script_tablas/tablas_def/tabla_37.html")


### Tabla 38: Especialidad del MR y MC (n y %), 2024
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
tabla_38 <- tibble::tibble(Especialidad = especialidades) %>%
  left_join(tabla_mr, by = c("Especialidad" = "especialidad_mr")) %>%
  left_join(tabla_mc, by = c("Especialidad" = "especialidad_mc"))

# Reemplazar NA por 0
tabla_38 <- tabla_38 %>%
  mutate(
    MR_Num = tidyr::replace_na(MR_Num, 0),
    MR_Pct = tidyr::replace_na(MR_Pct, 0),
    MC_Num = tidyr::replace_na(MC_Num, 0),
    MC_Pct = tidyr::replace_na(MC_Pct, 0)
  )

# Añadir fila total
fila_total <- tibble::tibble(
  Especialidad = "Total",
  MR_Num = sum(tabla_38$MR_Num, na.rm = TRUE),
  MR_Pct = 100,
  MC_Num = sum(tabla_38$MC_Num, na.rm = TRUE),
  MC_Pct = 100
)
tabla_38 <- bind_rows(tabla_38, fila_total)

# Ordenar las filas: penúltima 'Otra', última 'No consta', luego 'Total'
orden_especialidades <- c(
  setdiff(tabla_38$Especialidad, c("Otra", "No consta", "Total")),
  "Otra", "No consta", "Total"
)
tabla_38 <- tabla_38 %>%
  mutate(Especialidad = factor(Especialidad, levels = orden_especialidades)) %>%
  arrange(Especialidad)

# Etiquetas para gt
labels_38 <- c(
  Especialidad = "Especialidad",
  MR_Num = "Num",
  MR_Pct = "%",
  MC_Num = "Num",
  MC_Pct = "%"
)

# Crear tabla gt 
t38 <- tabla_38 %>%
  gt() %>%
  tab_header(
    title = "Especialidad del MR y MC",
    subtitle = "Num y %, 2024"
  ) %>%
  cols_label(.list = labels_38) %>%
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

gtsave(t38, "script_tablas/tablas_def/tabla_38.html")


### Tabla 42: Tiempos del procedimiento por CC. AA. 2021-2024 (media y mediana)
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
    tiempo_2sol_mr = ifelse(
      !is.na(fecha_informe_mr) & !is.na(fecha_segunda_solicitud) & fecha_informe_mr >= fecha_segunda_solicitud,
      as.numeric(fecha_informe_mr - fecha_segunda_solicitud),
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
    "tiempo_2sol_mr",
    "tiempo_mc_cgye",
    "tiempo_cgye_prestacion",
    "tiempo_reclamacion_resolucion"
  ),
  Media = c(
    round(mean(df_tiempos$tiempo_1sol_prestacion, na.rm = TRUE), 2),
    round(mean(df_tiempos$tiempo_1sol_2sol, na.rm = TRUE), 2),
    round(mean(df_tiempos$tiempo_2sol_mr, na.rm = TRUE), 2),
    round(mean(df_tiempos$tiempo_mc_cgye, na.rm = TRUE), 2),
    round(mean(df_tiempos$tiempo_cgye_prestacion, na.rm = TRUE), 2),
    round(mean(df_tiempos$tiempo_reclamacion_resolucion, na.rm = TRUE), 2)
  ),
  Mediana = c(
    round(median(df_tiempos$tiempo_1sol_prestacion, na.rm = TRUE), 2),
    round(median(df_tiempos$tiempo_1sol_2sol, na.rm = TRUE), 2),
    round(median(df_tiempos$tiempo_2sol_mr, na.rm = TRUE), 2),
    round(median(df_tiempos$tiempo_mc_cgye, na.rm = TRUE), 2),
    round(median(df_tiempos$tiempo_cgye_prestacion, na.rm = TRUE), 2),
    round(median(df_tiempos$tiempo_reclamacion_resolucion, na.rm = TRUE), 2)
  )
)

# Mostrar la tabla
print(tabla_tiempos)

# Hacer tabla gt con el resto de datos manuales
tabla_42 <- tibble::tibble(
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
  Media_2024 = c(82.60, 22.10, 13.40, 15.30, 27.40, 25.60),
  Mediana_2024 = c(62, 16, 5, 10, 15, 19)
)

labels_42 <- c(
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

t42 <- tabla_42 %>%
  gt() %>%
  tab_header(
    title = "Tiempos de procedimiento, histórico 2021-2024"
  ) %>%
  cols_label(.list = labels_42) %>%
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

gtsave(t42, "script_tablas/tablas_def/tabla_42.html")


###Tabla 43: Solicitudes, solicitudes aprobadas, aplazamientos y tasa de aplazamientos por CC.AA 2024.
tabla_43 <- df %>%
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
  Solicitudes = sum(tabla_43$Solicitudes, na.rm = TRUE),
  Solicitudes_aprobadas = sum(tabla_43$Solicitudes_aprobadas, na.rm = TRUE),
  Aplazamientos = sum(tabla_43$Aplazamientos, na.rm = TRUE),
  Tasa_aplazamientos_aprobadas = round(sum(tabla_43$Aplazamientos, na.rm = TRUE) / ifelse(sum(tabla_43$Solicitudes_aprobadas, na.rm = TRUE) == 0, NA, sum(tabla_43$Solicitudes_aprobadas, na.rm = TRUE)) * 100, 2)
)
tabla_43 <- bind_rows(tabla_43, fila_total)

# Etiquetas para gt
labels_43 <- c(
  ccaa = "CC. AA.",
  Solicitudes = "Solicitudes",
  Solicitudes_aprobadas = "Solicitudes aprobadas",
  Aplazamientos = "Aplazamientos",
  Tasa_aplazamientos_aprobadas = "Tasa de aplazamientos por solicitudes aprobadas (%)"
)

t43 <- tabla_43 %>%
  gt() %>%
  tab_header(
    title = "Solicitudes, solicitudes aprobadas, aplazamientos y tasa de aplazamientos por CC.AA. 2024"
  ) %>%
  cols_label(.list = labels_43) %>%
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

gtsave(t43, "script_tablas/tablas_def/tabla_43.html")


### Tabla 44: Aplazamientos por sexo y edad (n y %), 2024
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
t44 <- aplazamientos_edad_gt %>%
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

gtsave(t44, "script_tablas/tablas_def/tabla_44.html")


###Tabla 45: Aplazamientos por enfermedad de base, (n y %) 2024
aplazamientos_base_ccaa <- df %>%
  filter(aplazamiento == "Si") %>%
  mutate(
    patologia = ifelse(is.na(patologia) | patologia == "", "No consta", patologia),
    ccaa = ifelse(is.na(ccaa) | ccaa == "" | ccaa == "PERDIDOS", "No consta", ccaa)
  ) %>%
  count(ccaa, patologia) %>%
  tidyr::pivot_wider(
    names_from = patologia,
    values_from = n,
    values_fill = 0
  )
# Añadir columna Total por fila
aplazamientos_base_ccaa <- aplazamientos_base_ccaa %>%
  mutate(Total = rowSums(select(., -ccaa)))
# Calcular el total general para porcentajes
suma_total_general <- sum(aplazamientos_base_ccaa$Total, na.rm =  TRUE)
# Definir el orden de las patologías
orden_columnas <- c(
  "Neurológica", "Oncológica", "Pluripatología", "Respiratoria",
  "Cardiovascular", "Otra", "No consta", "Total"
)
columnas_existentes <- intersect(orden_columnas, colnames(aplazamientos_base_ccaa))
# Añadir columnas de porcentaje solo para las patologías principales
patologias_pct <- setdiff(columnas_existentes, c("Otra", "No consta", "Total"))
for (col in patologias_pct) {
  aplazamientos_base_ccaa[[paste0(col, "_%")]] <- round(aplazamientos_base_ccaa[[col]] / suma_total_general * 100, 2)
}
# Intercalar columnas número y porcentaje, pero 'Otra', 'No consta' y 'Total' solo número
nombres_finales <- c("ccaa")
for (col in columnas_existentes) {
  if (col %in% c("Otra", "No consta", "Total")) {
    nombres_finales <- c(nombres_finales, col)
  } else {
    nombres_finales <- c(nombres_finales, col, paste0(col, "_%"))
  }
}
aplazamientos_base_ccaa <- aplazamientos_base_ccaa %>% select(all_of(nombres_finales))
# Añadir fila total nacional
fila_total <- aplazamientos_base_ccaa %>%
  select(-ccaa) %>%
  summarise(across(everything(), sum, na.rm = TRUE))
fila_total <- cbind(ccaa = "Total", fila_total)
# Los porcentajes de la fila total son la suma de los porcentajes de cada columna principal
for (col in patologias_pct) {
  fila_total[[paste0(col, "_%")]] <- round(sum(aplazamientos_base_ccaa[[paste0(col, "_%")]], na.rm = TRUE), 2)
}
aplazamientos_base_ccaa <- bind_rows(aplazamientos_base_ccaa, fila_total)
# Etiquetas para gt
labels_cols <- c(ccaa = "CC. AA.")
for (col in columnas_existentes) {
  if (col %in% c("Otra", "No consta", "Total")) {
    labels_cols <- c(labels_cols, setNames(col, col))
  } else {
    labels_cols <- c(labels_cols, setNames(c("Num", "%"), c(col, paste0(col, "_%"))))
  }
}
# Crear tabla gt con doble columna por patología principal y solo número para las tres últimas
t45 <- aplazamientos_base_ccaa %>%
  gt() %>%
  tab_header(
    title = "Aplazamientos por enfermedad de base y CC.AA.",
    subtitle = "Número y porcentaje por enfermedad de base y totales"
  ) %>%
  cols_label(.list = labels_cols) %>%
  cols_align(
    align = "center",
    columns = -ccaa
  ) %>%
  tab_spanner_delim(delim = "_") %>%
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

gtsave(t45, "script_tablas/tablas_def/tabla_45.html")



### Tabla 46: Tasa de aplazamientos frente a solicitudes totales (n y %), histórico 2021-2024
tabla_46 <- tibble::tibble(
  Año = c("2021", "2022", "2023", "2024"),
  Aplazamientos_totales = c(6, 22, 33, 68),  
  Solicitudes_totales = c(173, 576, 766, 977)   
)

# Calcular tasa de aplazamientos por año
tabla_46 <- tabla_46 %>%
  mutate(
    Tasa_aplazamientos = round(Aplazamientos_totales / Solicitudes_totales * 100, 2)
  )

# Añadir fila total
fila_total_46 <- tibble::tibble(
  Año = "Total",
  Aplazamientos_totales = sum(tabla_46$Aplazamientos_totales, na.rm = TRUE),
  Solicitudes_totales = sum(tabla_46$Solicitudes_totales, na.rm = TRUE),
  Tasa_aplazamientos = round(sum(tabla_46$Aplazamientos_totales, na.rm = TRUE) / sum(tabla_46$Solicitudes_totales, na.rm = TRUE) * 100, 2)
)
tabla_46 <- dplyr::bind_rows(tabla_46, fila_total_46)

# Etiquetas para gt
labels_46 <- c(
  Año = "Año",
  Aplazamientos_totales = "Aplazamientos totales",
  Solicitudes_totales = "Solicitudes totales",
  Tasa_aplazamientos = "Tasa de aplazamientos (%)"
)

library(gt)
t46 <- tabla_46 %>%
  gt() %>%
  tab_header(
    title = "Tasa de aplazamientos frente a solicitudes totales",
    subtitle = "Num y %, histórico 2021-2024"
  ) %>%
  cols_label(.list = labels_46) %>%
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

gtsave(t46, "script_tablas/tablas_def/tabla_46.html")


### Tabla 47: Tasa de acortamientos por solicitudes, histórico 2022-2024 (transpuesta)
tabla_47 <- tibble::tibble(
  Indicador = c("Solicitudes totales", "Acortamientos", "Tasa de acortamientos por solicitudes (%)"),
  `2022` = c(576, 82, round(82 / 576 * 100, 2)),
  `2023` = c(68, 766, round(68 / 766 * 100, 2)),
  `2024` = c(124, 977, round(124 / 977 * 100, 2))
)

t47 <- tabla_47 %>%
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

gtsave(t47, "script_tablas/tablas_def/tabla_47.html")

### Tabla 48: Frecuencia de Donaciones, extracciones y trasplantes de órganos tras la PAM por años 2021-24
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
t48 <- df_donantes_organos_año_t %>%
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

gtsave(t48, "script_tablas/tablas_def/tabla_48.html")
