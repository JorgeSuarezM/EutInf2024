## Ajustes manuales
df <- df %>%
    mutate(denegado = if_else(codigo == "NE00002187", 1, denegado),
        denegado = if_else(codigo == "NE00002204", 1, denegado),
        denegado = if_else(codigo == "NE00001664", 1, denegado),
        denegado = if_else(codigo == "542054927", 1, denegado),
        fallecimiento_tramitacion = if_else(codigo == "407816490", 1, fallecimiento_tramitacion),
        fallecimiento_tramitacion = if_else(codigo == "NE00001626", 1, fallecimiento_tramitacion))

# ---------- Comprobaciones de datos ---------- #
# Solicitudes
solicitudes<-df %>%
    select(codigo)
# Prestaciones
prestaciones<-df %>%
    filter(!is.na(fecha_prestacion)) %>%
    select(codigo)
# Denegaciones
denegaciones<-df %>%
    filter(denegado == 1) %>%
    select(codigo)
# Revocaciones
revocaciones<-df %>%
    filter(revocacion == "Si") %>%
    select(codigo)
# Muerte durante tramitación
muerte_tramitacion<-df %>%
    filter(fallecimiento_tramitacion == 1) %>%
    select(codigo)

filtro <- solicitudes %>%
    filter(!codigo %in% prestaciones$codigo)
filtro <- filtro %>%
    filter(!codigo %in% muerte_tramitacion$codigo)
filtro <- filtro %>%
    filter(!codigo %in% revocaciones$codigo)
filtro <- filtro %>%
    filter(!codigo %in% denegaciones$codigo)

filtro$codigo

## Comprobación Andalucía manual
solicitudesAND <- df %>%
    filter(ccaa == "Andalucía") %>%
    select(codigo)
# Prestaciones Andalucía
prestacionesAND <- df %>%
    filter(ccaa == "Andalucía" & !is.na(fecha_prestacion)) %>%
    select(codigo)
# Denegaciones Andalucía
denegacionesAND <- df %>%
    filter(ccaa == "Andalucía" & denegado == 1) %>%
    select(codigo)
# Revocaciones Andalucía
revocacionesAND <- df %>%
    filter(ccaa == "Andalucía" & revocacion == "Si") %>%
    select(codigo)
# Muerte durante tramitación Andalucía
muerte_tramitacionAND <- df %>%
    filter(ccaa == "Andalucía" & fallecimiento_tramitacion == 1) %>%
    select(codigo)

write.csv(solicitudesAND, "solicitudesAND.csv")
write.csv(prestacionesAND, "prestacionesAND.csv")
write.csv(denegacionesAND, "denegacionesAND.csv")
write.csv(revocacionesAND, "revocacionesAND.csv")
write.csv(muerte_tramitacionAND, "muerte_tramitacionAND.csv")



## Comprobación Canarias manual
solicitudesCAN <- df %>%
    filter(ccaa == "Canarias") %>%
    select(codigo)
# Prestaciones Canarias
prestacionesCAN <- df %>%
    filter(ccaa == "Canarias" & !is.na(fecha_prestacion)) %>%
    select(codigo)
# Denegaciones Canarias
denegacionesCAN <- df %>%
    filter(ccaa == "Canarias" & denegado == 1) %>%
    select(codigo)
# Revocaciones Canarias
revocacionesCAN <- df %>%
    filter(ccaa == "Canarias" & revocacion == "Si") %>%
    select(codigo)
# Muerte durante tramitación Canarias
muerte_tramitacionCAN <- df %>%
    filter(ccaa == "Canarias" & fallecimiento_tramitacion == 1) %>%
    select(codigo)

write.csv(solicitudesCAN, "solicitudesCAN.csv")
write.csv(prestacionesCAN, "prestacionesCAN.csv")
write.csv(denegacionesCAN, "denegacionesCAN.csv")
write.csv(revocacionesCAN, "revocacionesCAN.csv")
write.csv(muerte_tramitacionCAN, "muerte_tramitacionCAN.csv")



## Comprobación Cataluña manual
solicitudesCAT <- df %>%
    filter(ccaa == "Cataluña") %>%
    select(codigo)
# Prestaciones Cataluña
prestacionesCAT <- df %>%
    filter(ccaa == "Cataluña" & !is.na(fecha_prestacion)) %>%
    select(codigo)
# Denegaciones Cataluña
denegacionesCAT <- df %>%
    filter(ccaa == "Cataluña" & denegado == 1) %>%
    select(codigo)
# Revocaciones Cataluña
revocacionesCAT <- df %>%
    filter(ccaa == "Cataluña" & revocacion == "Si") %>%
    select(codigo)
# Muerte durante tramitación Cataluña
muerte_tramitacionCAT <- df %>%
    filter(ccaa == "Cataluña" & fallecimiento_tramitacion == 1) %>%
    select(codigo)

write.csv(solicitudesCAT, "solicitudesCAT.csv")
write.csv(prestacionesCAT, "prestacionesCAT.csv")
write.csv(denegacionesCAT, "denegacionesCAT.csv")
write.csv(revocacionesCAT, "revocacionesCAT.csv")
write.csv(muerte_tramitacionCAT, "muerte_tramitacionCAT.csv")

den <- as.vector(denegacionesCAT$codigo)
mue <- as.vector(muerte_tramitacionCAT$codigo)
t1 <- data.frame(mue = mue)
t2 <- data.frame(den = den)
intersect_codes <- intersect(t1$mue, t2$den)
intersect_codes

df %>%
    filter(codigo == "407816490") %>%
    select(codigo)


## Comprobación Navarra manual
solicitudesNAV <- df %>%
    filter(ccaa == "Navarra") %>%
    select(codigo)
# Prestaciones Navarra
prestacionesNAV <- df %>%
    filter(ccaa == "Navarra" & !is.na(fecha_prestacion)) %>%
    select(codigo)
# Denegaciones Navarra
denegacionesNAV <- df %>%
    filter(ccaa == "Navarra" & denegado == 1) %>%
    select(codigo)
# Revocaciones Navarra
revocacionesNAV <- df %>%
    filter(ccaa == "Navarra" & revocacion == "Si") %>%
    select(codigo)
# Muerte durante tramitación Navarra
muerte_tramitacionNAV <- df %>%
    filter(ccaa == "Navarra" & fallecimiento_tramitacion == 1) %>%
    select(codigo)

write.csv(solicitudesNAV, "solicitudesNAV.csv")
write.csv(prestacionesNAV, "prestacionesNAV.csv")
write.csv(denegacionesNAV, "denegacionesNAV.csv")
write.csv(revocacionesNAV, "revocacionesNAV.csv")
write.csv(muerte_tramitacionNAV, "muerte_tramitacionNAV.csv")

den <- as.vector(denegacionesNAV$codigo)
mue <- as.vector(muerte_tramitacionNAV$codigo)
t1 <- data.frame(mue = mue)
t2 <- data.frame(den = den)
intersect_codes <- intersect(t1$mue, t2$den)
intersect_codes

## Murcia
t <- df %>% filter(codigo == "NE00000764")
t$denegado
t$fallecimiento_tramitacion