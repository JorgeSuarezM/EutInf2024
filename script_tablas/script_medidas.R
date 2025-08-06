# Número de fallecimientos tras aplazamiento
n_fallecimientos_aplazamiento <- sum(df$aplazamiento == "Si" & df$fallecimiento_tramitacion == 1, na.rm = TRUE)
# Total de aplazamientos
n_aplazamientos <- sum(df$aplazamiento == "Si", na.rm = TRUE)
# Tasa (%)
tasa_fallecimientos_aplazamiento <- round(n_fallecimientos_aplazamiento / n_aplazamientos * 100, 2)

# Número de prestaciones tras aplazamiento
n_prestaciones_aplazamiento <- sum(df$aplazamiento == "Si" & !is.na(df$fecha_prestacion), na.rm = TRUE)
# Tasa (%)
tasa_prestaciones_aplazamiento <- round(n_prestaciones_aplazamiento / n_aplazamientos * 100, 2)

print(n_fallecimientos_aplazamiento)
print(tasa_fallecimientos_aplazamiento)
print(n_prestaciones_aplazamiento)
print(tasa_prestaciones_aplazamiento)

## Muertes durante el procedimiento
sum(df$fallecimiento_tramitacion == 1 & df$informe_cgye == "Si", na.rm = TRUE)
sum(df$fallecimiento_tramitacion == 1 & df$informe_cgye == "No", na.rm = TRUE)
sum(df$fallecimiento_tramitacion == 1 & df$informe_cgye == "", na.rm = TRUE)

## Tiempo entre 1º solicitud y fallecimiento durante tramitación (solo si hay ambas fechas, sin agrupar)
df_tiempos <- df %>%
  mutate(
    tiempo_1sol_muerte = ifelse(
      !is.na(fecha_fallecimiento) & !is.na(fecha_primera_solicitud),
      as.numeric(fecha_fallecimiento - fecha_primera_solicitud),
      NA
    )
  )

# Filtrar solo los casos con ambas fechas
tiempos_validos <- df_tiempos %>%
  filter(!is.na(tiempo_1sol_muerte))

media_tiempo <- round(mean(tiempos_validos$tiempo_1sol_muerte, na.rm = TRUE), 2)
mediana_tiempo <- round(median(tiempos_validos$tiempo_1sol_muerte, na.rm = TRUE), 2)

print(media_tiempo)
print(mediana_tiempo)

###
sum(df$informe_cgye == "Si" & df$fallecimiento_tramitacion == 1, na.rm = TRUE)
