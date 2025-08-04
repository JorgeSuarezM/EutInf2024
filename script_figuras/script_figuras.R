library(tidyverse)
library(mapSpain)
library(sf)

mapCCAA <- esp_get_ccaa() %>%
    select(iso2.ccaa.name.es, geometry) %>%
    rename(ccaa = iso2.ccaa.name.es) %>%
    mutate(ccaa = case_when(
        ccaa == "Andalucía" ~ "Andalucía",
        ccaa == "Aragón" ~ "Aragón",
        ccaa == "Asturias, Principado de" ~ "Asturias",
        ccaa == "Islas Baleares" ~ "Islas Baleares",
        ccaa == "Canarias" ~ "Canarias",
        ccaa == "Cantabria" ~ "Cantabria",
        ccaa == "Castilla-La Mancha" ~ "Castilla-La Mancha",
        ccaa == "Castilla y León" ~ "Castilla y León",
        ccaa == "Cataluña" ~ "Cataluña",
        ccaa == "Extremadura" ~ "Extremadura",
        ccaa == "Galicia" ~ "Galicia",
        ccaa == "Madrid, Comunidad de" ~ "Madrid",
        ccaa == "Murcia, Región de" ~ "Murcia",
        ccaa == "Navarra, Comunidad Foral de" ~ "Navarra",
        ccaa == "La Rioja" ~ "La Rioja",
        ccaa == "País Vasco" ~ "País Vasco",
        ccaa == "Valenciana, Comunidad" ~ "Comunidad Valenciana",
        ccaa == "Ceuta" ~ "Ceuta",
        ccaa == "Melilla" ~ "Melilla",
    ))

colors = c("#fdbb63", "#6b174a", "#79d1f1", "#138fc2", "#ee2d71", "#00465e", "#f5a4c8")
colors_gradient = colorRampPalette(c("white", "#fdbb63"))(19)

# (AP. 4.1.) Figura 1: Mapa de solicitudes por CC. AA. 2024, (puntos)
fig1<-df %>%
  select(ccaa) %>%
  group_by(ccaa) %>%
  summarise(n=n())
fig1 <- add_row(fig1, ccaa = "Ceuta", n = 0)
fig1 <- full_join(fig1, mapCCAA, by = "ccaa")

fig1_centroids <- st_centroid(fig1$geometry)
fig1_points <- fig1 %>%
  mutate(centroid = fig1_centroids) %>%
  st_as_sf()

ggplot(fig1) +
    geom_sf(aes(geometry = geometry, fill = log2(n)), color = "white") +
    geom_sf(data = fig1_points, aes(geometry = centroid, size = n), color = "#6b174a", alpha = .8) +
    scale_size_continuous(range = c(0, 22), name = "Nº Solicitudes") +
    scale_fill_gradientn(
        colours = colors_gradient,
        na.value = "grey90",
        name = "Solicitudes"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA)
    ) +
    labs(
        title = "Solicitudes por CC. AA. 2024",
        fill = "Nº Solicitudes"
    )

ggsave("script_figuras/figuras_def/fig1.png", width = 10, height = 8, dpi = 300, bg = "transparent")

# (AP. 4.2.) Figura 2: Solicitudes por tramo de edad 2024

# (AP. 4.2.) Figura 3: Solicitudes por sexo y CC. AA. 2024 (barras, sin total)

# (AP. 4.2.) Figura 4: Solicitudes por enfermedad de base 2024 (barras, sin total)

# (AP. 4.2.) Figura 5: Solicitudes nativos/extranjeros 2024 (barras, sin total)

# (AP. 4.2.) Figura 6: Mapa de tasa de solicitudes por 100k habitantes por CC. AA. 2024 (escala de colores)

# (AP. 5.1.) Figura 7: Mapa de prestaciones por CC. AA. 2024, (puntos)

# (AP. 5.2.) Figura 8: Prestaciones por CC. AA. y tramo de edad 2024 (barras apiladas por CC. AA.)

# (AP. 5.3.) Figura 9: Prestaciones por lugar de prestación y CC. AA. 2024 (barras apiladas por CC. AA.)

# (AP. 5.3.) Figura 10: Prestaciones por ámbito de prestación público-privado y CC. AA. 2024 (barras apiladas por CC. AA.)

# (AP. 6) Figura 11: Mapa de denegaciones por CC. AA. (puntos)

# (AP. 7.1) Figura 12: Mapa de fallecimientos durante tramitación por CC. AA. (puntos)

# (AP. 7.2) Figura 13: Fallecimientos durante tramitación por CC. AA. y tramo de edad 2024 (barras apiladas por CC. AA.)

# (AP. 8.1) Figura 14: Mapa de revocaciones por CC. AA. (puntos)

# (AP. 9.1) Figura 15: Especialidad del MR y MC 2024 (barras)

# (AP. 9.2) Figura 16: Histórico 2021-2024 de media de plazos en cada fase (barras apiladas, eje X=media de días)
