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

mapCCAA <- full_join(mapCCAA, habitantes_ccaa, by="ccaa")

colors = c("#fdbb63", "#6b174a", "#79d1f1", "#138fc2", "#ee2d71", "#00465e", "#f5a4c8")
colors_gradient = colorRampPalette(c("white", "#fdbb63"))(19)
colors_gradient8 = colorRampPalette(c("#fdbb63", "#6b174a"))(8)
colors_gradient20 = colorRampPalette(c("#fdbb63", "#6b174a"))(20)

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
    geom_sf(aes(geometry = geometry, fill = log2(n)), color = "black") +
    geom_sf(data = fig1_points, aes(geometry = centroid, size = n), color = "#6b174a", alpha = .8) +
    scale_size_continuous(
        range = c(0, 18),
        name = "Solicitudes"
    ) +
    scale_fill_gradientn(
        colors = colors_gradient,
        name = "Solicitudes",
        labels = function(x) round(50*x, 1)
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
    ) +
    geom_sf(aes(geometry = geometry), fill = NA, color = "black", size = 0.5)

ggsave("script_figuras/figuras_def/fig1.png", width = 10, height = 8, dpi = 300, bg = "transparent")

# (AP. 4.2.) Figura 2: Solicitudes por tramo de edad 2024
fig2<-table(df$edad)
ggplot(df, aes(x=factor(edad, levels=c("<30", "30-39", "40-49", "50-59", "60-69", "70-79", ">80")))) +
    geom_bar(width = 1, aes(fill = ..count..), color="black") +
    scale_fill_gradient(
        low = "#fdbb63",
        high = "#6b174a"
    ) +
    geom_label(
        stat = "count",
        aes(label = ..count.., fill = ..count..),
        color = "black",
        size = 5,
        label.size = 0.5,
        label.r = unit(0.15, "lines"),
        vjust = -0.5,
        alpha = 0.5
    ) +
    theme_classic()+
    labs(x = element_blank(), y = element_blank()) +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 16),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 285)) +
    guides(fill = "none") +
    labs(title = "Solicitudes por tramo de edad 2024") +
    theme(plot.title = element_text(size = 24))

ggsave("script_figuras/figuras_def/fig2.png", width = 8, height = 12, dpi = 300, bg = "transparent")

# (AP. 4.2.) Figura 3: Solicitudes por sexo y CC. AA. 2024 (barras, sin total)
ggplot(df, aes(x = ccaa, fill = sexo)) +
    geom_bar(position = "dodge") +
    theme_classic() +
    labs(x = element_blank(), y = element_blank(), fill = "Sexo") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
    scale_fill_manual(values = c("H" = "#fdbb63", "M" = "#6b174a"), labels = c("H" = "Hombre", "M" = "Mujer")) +
    theme(
        legend.position = c(.9, .9),
        legend.background = element_rect(fill = "transparent", color = "black", size = 1),
        legend.text = element_text(size = 16, margin = margin(t = 10, b = 10, l = 10, r = 10)),
        legend.title = element_text(size = 18, margin = margin(b = 15))
    ) +
    geom_label(
        stat = "count",
        aes(label = ..count.., y = ..count.., fill = sexo),
        position = position_dodge(width = .9),
        color = "black",
        size = 2.5, # reducido el tamaño de las etiquetas
        label.size = 0.3,
        alpha = 0.5,
        show.legend = FALSE,
        vjust = -0.4
    ) +
    theme(
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        plot.margin = margin(r = 20, l = 20, t=20),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
    ) +
    labs(title = "Solicitudes por sexo y CC. AA. 2024") +
    theme(plot.title = element_text(size = 24))

ggsave("script_figuras/figuras_def/fig3.png", width = 8, height = 12, dpi = 300, bg = "transparent")

# (AP. 4.2.) Figura 4: Solicitudes por enfermedad de base 2024 (barras, sin total)
df <- df %>% mutate(patologia = if_else(patologia == "", "No consta", patologia),
    patologia = if_else(patologia == "Reumatología y patología osteomuscular", "Reumatología/osteomuscular", patologia))
ggplot(df, aes(x = factor(patologia, levels=c("Neurológica", "Oncológica", "Cardiovascular", "Respiratoria", "Reumatología/osteomuscular", "Pluripatología", "Otra", "No consta")))) + 
    geom_bar(width = 1, aes(fill = patologia))+
    scale_fill_manual(
        values = c(
            "Neurológica" = "#FDBB63",
            "Oncológica" = "#E8A35F",
            "Cardiovascular" = "#D38C5B",
            "Respiratoria" = "#BE7458",
            "Reumatología/osteomuscular" = "#A95D54",
            "Pluripatología" = "#944551",
            "Otra" = "#7F2E4D",
            "No consta" = "#6B174A"
        )
    ) +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 320)) +
    labs(x = element_blank(), y = element_blank()) +
    geom_label(
        stat = "count",
        aes(label = ..count.., fill = patologia, y = ..count..),
        color = "black",
        size = 4,
        label.size = 0.5,
        alpha = 0.5,
        show.legend = FALSE,
        vjust = -0.4
    ) +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size = 24),
        plot.margin = margin(t = 20, b = 20)
    ) +
    labs(title = "Solicitudes por enfermedad de base 2024")
    
ggsave("script_figuras/figuras_def/fig4.png", width = 10, height = 12, dpi = 300, bg = "transparent")

# (AP. 4.2.) Figura 5a: Solicitudes nativos/extranjeros 2024 (barras, sin total)
df %>% 
    mutate(nativo = if_else(pais_nacimiento == "España", "España", "Extranjero")) %>%
ggplot(., aes(x = factor(nativo, levels = c("España", "Extranjero")), fill = nativo)) +
    geom_bar(width = 1) +
    theme_classic() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(table(df$pais_nacimiento == "España")))) +
    labs(x = element_blank(), y = element_blank()) +
    theme(
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none"
    ) +
    scale_fill_manual(values = c("España" = "#fdbb63", "Extranjero" = "#6b174a")) +
    geom_label(
        stat = "count",
        aes(label = ..count.., y = ..count.., fill = nativo),
        color = "black",
        fill = "white",
        alpha = 0.5,
        size = 6,
        label.size = 0.5,
        show.legend = FALSE,
        vjust = 1.5
    ) +
    labs(title = "Solicitudes nativos/extranjeros 2024") +
    theme(
        plot.title = element_text(size = 24, margin = margin(b = 20)),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
    )

ggsave("script_figuras/figuras_def/fig5a.png", width = 8, height = 12, dpi = 300, bg = "transparent")

# (AP. 4.2.) Figura 5b: Solicitudes extranjeros por país de nacimiento 2024 (barras, sin total)
df %>% filter(pais_nacimiento!="España" & pais_nacimiento!="") %>%
ggplot(., aes(x = pais_nacimiento, fill = pais_nacimiento)) +
    geom_bar(width = 1) +
    theme_classic() +
    labs(x = element_blank(), y = element_blank()) +
    scale_x_discrete(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 9.5)) +
    scale_fill_manual(
        values = c(
        "Alemania" = "#FDBB63", 
        "Argentina" = "#F5B261",
        "Bélgica" = "#EDA960",
        "Bolivia" = "#E5A15F",
        "Chile" = "#DE985D",
        "China" = "#D68F5C",
        "Colombia" = "#CE875B",
        "Cuba" = "#C77E59",
        "Dinamarca" = "#BF7558",
        "Francia" = "#B76D57",
        "Irlanda" = "#B06455",
        "Italia" = "#A85C54",
        "Noruega" = "#A05353",
        "Países Bajos" = "#994A51",
        "Reino Unido" = "#914250",
        "República Dominicana" = "#89394F",
        "Rumanía" = "#82304D",
        "Suiza" = "#7A284C",
        "Uruguay" = "#721F4B",
        "Venezuela" = "#6B174A"
        )
    ) +
    geom_label(
        stat = "count",
        aes(label = ..count.., y = ..count.., fill = pais_nacimiento),
        color = "black",
        alpha = 0.5,
        label.size = 0.5,
        show.legend = FALSE,
        vjust = -0.4
    ) +
    theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        plot.margin = margin(t = 10, r = 30, b = 10, l = 30),
        plot.title = element_text(size = 24),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
    ) +
    labs(title = "Solicitudes extranjeros por país de nacimiento 2024")

ggsave("script_figuras/figuras_def/fig5b.png", width = 12, height = 10, dpi = 300, bg = "transparent")

# (AP. 4.2.) Figura 6: Mapa de tasa de solicitudes por 100k habitantes por CC. AA. 2024 (escala de colores)
fig6<-df %>%
  select(ccaa) %>%
  group_by(ccaa) %>%
  summarise(n=n())
fig6 <- add_row(fig6, ccaa = "Ceuta", n = 0)
fig6 <- full_join(fig6, mapCCAA, by = "ccaa")
fig6 <- fig6 %>% mutate(hab100k= habitantes/100000,
    tasa = (n/hab100k)*100)

fig6_centroids <- st_centroid(fig6$geometry)
fig6_points <- fig6 %>%
  mutate(centroid = fig6_centroids) %>%
  st_as_sf()

ggplot(fig6) +
    geom_sf(aes(geometry = geometry, fill = tasa), color = "black") +
    geom_sf(data = fig6_points, aes(geometry = centroid, size = log(tasa, base = 5)), color = "#6b174a", alpha = .8) +
    scale_size_continuous(
        range = c(0, 10),
        name = "Tasa por cada 100 000 hab.",
        breaks = c(log(100, base = 5), log(250, base=5), log(400, base=5)),
        labels = function(x) round(5^x, 1)
    ) +
    scale_fill_gradientn(
        colours = colors_gradient,
        na.value = "grey90",
        name = "Tasa por cada 100 000 hab."
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
        title = "Solicitudes por cada 100 000 habitantes 2024",
        fill = "Tasa por cada 100 000 hab."
    )

ggsave("script_figuras/figuras_def/fig6.png", width = 10, height = 8, dpi = 300, bg = "transparent")

# (AP. 5.1.) Figura 7: Mapa de prestaciones por CC. AA. 2024, (puntos)
fig7<-df %>%
    mutate(prestacion=if_else(!is.na(fecha_prestacion), 1, 0)) %>%
    filter(prestacion==1) %>%
    select(ccaa) %>%
    group_by(ccaa) %>%
    summarise(n=n())
fig7 <- add_row(fig7, ccaa = "Ceuta", n = 0)
fig7 <- full_join(fig7, mapCCAA, by = "ccaa")

fig7_centroids <- st_centroid(fig7$geometry)
fig7_points <- fig7 %>%
  mutate(centroid = fig7_centroids) %>%
  st_as_sf()

ggplot(fig7) +
    geom_sf(aes(geometry = geometry, fill = log2(n)), color = "black") +
    geom_sf(data = fig7_points, aes(geometry = centroid, size = n), color = "#6b174a", alpha = .8) +
    scale_size_continuous(
        range = c(0, 18),
        name = "Prestaciones"
    ) +
    scale_fill_gradientn(
        colors = colors_gradient,
        name = "Prestaciones",
        labels = function(x) round(20*x, 1)
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
        title = "Prestaciones por CC. AA. 2024",
        fill = "Nº Prestaciones"
    ) +
    geom_sf(aes(geometry = geometry), fill = NA, color = "black", size = 0.5)

ggsave("script_figuras/figuras_def/fig7.png", width = 10, height = 8, dpi = 300, bg = "transparent")

# (AP. 5.2.) Figura 8: Prestaciones por CC. AA. y tramo de edad 2024 (barras apiladas por CC. AA.)

# (AP. 5.3.) Figura 9: Prestaciones por lugar de prestación y CC. AA. 2024 (barras apiladas por CC. AA.)

# (AP. 5.3.) Figura 10: Prestaciones por ámbito de prestación público-privado y CC. AA. 2024 (barras apiladas por CC. AA.)

# (AP. 6) Figura 11: Mapa de denegaciones por CC. AA. (puntos)

# (AP. 7.1) Figura 12: Mapa de fallecimientos durante tramitación por CC. AA. (puntos)

# (AP. 7.2) Figura 13: Fallecimientos durante tramitación por CC. AA. y tramo de edad 2024 (barras apiladas por CC. AA.)

# (AP. 8.1) Figura 14: Mapa de revocaciones por CC. AA. (puntos)

# (AP. 9.1) Figura 15: Especialidad del MR y MC 2024 (barras)

# (AP. 9.2) Figura 16: Histórico 2021-2024 de media de plazos en cada fase (barras apiladas, eje X=media de días)


