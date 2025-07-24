##En este script se van a agrupar las tablas al final para facilitar su exportación

# Apartado 3: Estadísticas generales sobre la PAM
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
  TotalSolicitantesP = round((sum(Solicitante) / nrow(t) * 100), digits=1),
  TotalPrestacionesN = sum(Beneficiario),
  TotalPrestacionesP = round((sum(Beneficiario) / nrow(t) * 100), digits=1),
  TotalMuerteDuranteTramitacionN = sum(fallecimiento_tramitacion, na.rm=T),
  TotalMuerteDuranteTramitacionP = round((sum(fallecimiento_tramitacion, na.rm=T) / nrow(t) * 100), digits=1),
  TotalDenegadosN = sum(denegado, na.rm = TRUE),
  TotalDenegadosP = round((sum(denegado, na.rm = TRUE) / nrow(t) * 100), digits=1),
  TotalRevocadosN = sum(revocacion, na.rm = TRUE),
  TotalRevocadosP = round((sum(revocacion, na.rm = TRUE) / nrow(t) * 100), digits=1)
)
tab1b<-t %>%
summarise(
    ccaa = "Total",
    TotalSolicitantesN = sum(Solicitante),
    TotalSolicitantesP = round((sum(Solicitante) / nrow(t) * 100), digits=1),
    TotalPrestacionesN = sum(Beneficiario),
    TotalPrestacionesP = round((sum(Beneficiario) / nrow(t) * 100), digits=1),
    TotalMuerteDuranteTramitacionN = sum(fallecimiento_tramitacion, na.rm=T),
    TotalMuerteDuranteTramitacionP = round((sum(fallecimiento_tramitacion, na.rm=T) / nrow(t) * 100), digits=1),
    TotalDenegadosN = sum(denegado, na.rm = TRUE),
    TotalDenegadosP = round((sum(denegado, na.rm = TRUE) / nrow(t) * 100), digits=1),
    TotalRevocadosN = sum(revocacion, na.rm = TRUE),
    TotalRevocadosP = round((sum(revocacion, na.rm = TRUE) / nrow(t) * 100), digits=1)
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
gtsave(t1, "script_tablas/tablas_def/tabla_1.png") #Para guardar en formato .png, aunque puede dar problemas si no se configura bien
gtsave(t1, "script_tablas/tablas_def/tabla_1.docx")

### Tabla 2: Distribución por edad de solicitantes y beneficiarios

### Tabla 3: Distribución por enfermedad de base de solicitantes y beneficiarios

## Estadísticas regionales
### Tabla 4: Distribución por CCAA de solicitantes, beneficiarios y denegaciones

### Tasa de mortalidad por PAM total y por CCAA