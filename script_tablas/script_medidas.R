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
