# Preparación de datos

## Librerías
library(tidyverse)
library(gt)

# Cargar los datos
df<-read.csv("data.csv", fileEncoding="latin1", sep=";")
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

# Crear variables de denegación y muerte durante la tramitación
df<-df %>%
mutate(Denegado=if_else(
    (Informe.favorable..m.dico.responsable == 0 | Informe.favorable.m.dico.consultor == 0 | Informe.favorable.Comisi.n.Garant.a.y.Evaluaci.n..CGyE. == 0) & is.na(FECHA_PRESTACION), 1, 0),
    muerte.durante.tramitacion=case_when(
        Fecha.de.Fallecimiento > Fecha.Informe.favorable.Comisi.n.Garant.a.y.Evaluaci.n..CGyE. ~ 0,
        (Fecha.de.Fallecimiento <= Fecha.Informe.favorable.Comisi.n.Garant.a.y.Evaluaci.n..CGyE. | Fecha.de.Fallecimiento <=Fecha.Informe.favorable.m.dico.consultor | Fecha.de.Fallecimiento <= Fecha.Informe.favorable.m.dico.responsable) ~ 1,
        !is.na(Fecha.de.Fallecimiento) & is.na(Fecha.Informe.favorable.m.dico.responsable) ~ 1
    ),
    muerte.durante.tramitacion.momento=case_when(
        muerte.durante.tramitacion == 0 ~ "No",
        Fecha.de.Fallecimiento <= Fecha.Informe.favorable.Comisi.n.Garant.a.y.Evaluaci.n..CGyE. ~ "1Previo a Comisión",
        Fecha.de.Fallecimiento <= Fecha.Informe.favorable.m.dico.consultor ~ "2Previo a Informe Médico Consultor",
        Fecha.de.Fallecimiento <= Fecha.Informe.favorable.m.dico.responsable ~ "3Previo a Informe Médico Responsable"
    )
)

# Verificar si la fecha de prestación es anterior a la fecha del informe favorable de la comisión
table(as.Date(df$FECHA_PRESTACION, format="%d/%m/%y") < as.Date(df$Fecha.Informe.favorable.Comisi.n.Garant.a.y.Evaluaci.n..CGyE., format="%d/%m/%y"))
