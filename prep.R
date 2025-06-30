# Preparación de datos

## Librerías
library(tidyverse)
library(gt)
library(gtExtras)

# Cargar los datos
df<-read.csv("data.csv", fileEncoding="UTF-8", sep=";")
glimpse(df)

# Establecer formato de fecha
df<-df %>%
mutate(FECHA_CIERRE = as.Date(FECHA_CIERRE, format="%d/%m/%y"),
FECHA_PRESTACION = as.Date(FECHA_PRESTACION, format="%d/%m/%y"),
Fecha.Informe.favorable.Comisi.n.Garant.a.y.Evaluaci.n..CGyE. = as.Date(Fecha.Informe.favorable.Comisi.n.Garant.a.y.Evaluaci.n..CGyE., format="%d/%m/%y"),
Fecha.Informe.favorable.m.dico.consultor = as.Date(Fecha.Informe.favorable.m.dico.consultor, format="%d/%m/%y"),
Fecha.Informe.favorable.m.dico.responsable = as.Date(Fecha.Informe.favorable.m.dico.responsable, format="%d/%m/%y"),
Fecha.de.Fallecimiento = as.Date(Fecha.de.Fallecimiento, format="%d/%m/%y"),
Fecha.prestacion = as.Date(FECHA_PRESTACION, format="%d/%m/%y"))

# Renombrar columnas para facilitar el uso
# (!) Comentar con ## las líneas siguientes para evitar conflictos con las tablas ya hechas
df<-df %>%
rename(
    InformeMR = Informe.favorable..médico.responsable,
    InformeMC = Informe.favorable.médico.consultor,
    InformeCGyE = Informe.favorable.Comisión.Garantía.y.Evaluación..CGyE.,
    FechaCGyE = Fecha.Informe.favorable.Comisión.Garantía.y.Evaluación..CGyE.,
    FechaMC = Fecha.Informe.favorable.médico.consultor,
    FechaMR = Fecha.Informe.favorable.médico.responsable,
    FechaFallecimiento = Fecha.de.Fallecimiento,
    FechaPrestacion = FECHA_PRESTACION,
    FechaCierre = FECHA_CIERRE
)

# Renombrar CCAA
df<-df%>%
mutate(CCAAor = CCAA,
    CCAA = case_when(
    CCAA == "ANDALUCÍA" ~ "Andalucía",
    CCAA == "ARAGÓN" ~ "Aragón",
    CCAA == "ASTURIAS, PRINCIPADO DE" ~ "Asturias",
    CCAA == "BALEARS, ILLES" ~ "Illes Balears",
    CCAA == "CANARIAS" ~ "Canarias",
    CCAA == "CANTABRIA" ~ "Cantabria",
    CCAA == "CASTILLA Y LEÓN" ~ "Castilla y León",
    CCAA == "CASTILLA-LA MANCHA" ~ "Castilla-La Mancha",
    CCAA == "CATALUÑA" ~ "Cataluña",
    CCAA == "COMUNITAT VALENCIANA" ~ "C. Valenciana",
    CCAA == "EXTREMADURA" ~ "Extremadura",
    CCAA == "GALICIA" ~ "Galicia",
    CCAA == "MADRID, COMUNIDAD DE" ~ "Madrid",
    CCAA == "MURCIA, REGIÓN DE" ~ "Murcia",
    CCAA == "NAVARRA, COMUNIDAD FORAL DE" ~ "Navarra",
    CCAA == "PAIS VASCO" ~ "País Vasco",
    CCAA == "RIOJA, LA" ~ "La Rioja",
    CCAA == "CEUTA" ~ "Ceuta",
    CCAA == "MELILLA" ~ "Melilla",
    CCAA == "" | is.na(CCAA) ~ "No consta"
))

# Crear variables de denegación y muerte durante la tramitación
df<-df %>%
mutate(Denegado=if_else(
    (InformeMR == 0 | InformeMC == 0 | InformeCGyE == 0) & FechaPrestacion!="", 1, 0),
    MuerteDuranteTramitacion=case_when(
        FechaFallecimiento > FechaCGyE ~ 0,
        FechaPrestacion!= "" ~ 0,
        (FechaFallecimiento <= FechaCGyE | FechaFallecimiento <= FechaMC | FechaFallecimiento <= FechaMR) ~ 1,
        !is.na(FechaFallecimiento) & (is.na(FechaPrestacion)|FechaPrestacion=="") ~ 1
    ),
    MuerteDuranteTramitacionMomento=case_when(
        MuerteDuranteTramitacion == 0 ~ "No",
        FechaFallecimiento <= FechaCGyE ~ "1Previo a Comisión",
        FechaFallecimiento <= FechaMC ~ "2Previo a Informe Médico Consultor",
        FechaFallecimiento <= FechaMR ~ "3Previo a Informe Médico Responsable"
    )
)

# Verificar si la fecha de prestación es anterior a la fecha del informe favorable de la comisión
table(as.Date(df$FechaPrestacion, format="%d/%m/%y") < as.Date(df$FechaCGyE, format="%d/%m/%y")) # 1 caso: CODIGO NE00001484
