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
colors_gradient7 = colorRampPalette(c("#fdbb63", "#6b174a"))(7)
colors_gradient4 = colorRampPalette(c("#fdbb63", "#6b174a"))(4)
colors_gradient6 = colorRampPalette(c("#fdbb63", "#6b174a"))(6)

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
# ccaa_order <- df %>% #Para ordenar las CC. AA. por número de solicitudes
#     group_by(ccaa) %>%
#     summarise(total = n()) %>%
#     arrange(desc(total)) %>%
#     pull(ccaa)

ggplot(df, aes(x = factor(ccaa, levels=ccaa_order), fill = sexo)) +
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

ggplot(df, aes(x = factor(patologia, levels=c("Neurológica", "Oncológica", "Pluripatología", "Respiratoria", "Cardiovascular", "Reumatología/osteomuscular", "Otra", "No consta")))) + 
    geom_bar(width = 1, aes(fill = patologia))+
    scale_fill_manual(
        values = c(
            "Neurológica" = "#FDBB63",
            "Oncológica" = "#E8A35F",
            "Pluripatología" = "#D38C5B",
            "Respiratoria" = "#BE7458",
            "Cardiovascular" = "#A95D54",
            "Reumatología/osteomuscular" = "#944551",
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
    
ggsave("script_figuras/figuras_def/fig4alt.png", width = 10, height = 12, dpi = 300, bg = "transparent")

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
df %>%
    filter(pais_nacimiento != "España" & pais_nacimiento != "") %>%
    count(pais_nacimiento) %>%
    arrange(desc(n)) %>%
    mutate(pais_nacimiento = factor(pais_nacimiento, levels = pais_nacimiento)) %>%
    ggplot(aes(x = pais_nacimiento, y = n, fill = pais_nacimiento)) +
        geom_bar(stat = "identity", width = 1) +
        theme_classic() +
        labs(x = element_blank(), y = element_blank()) +
        scale_x_discrete(expand = c(0, 0)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0, 9.5)) +
        scale_fill_manual(
                values = c(
                "Reino Unido" = "#FDBB63",
                "Alemania" = "#F5B261",
                "Venezuela" = "#EDA960",
                "Bélgica" = "#E5A15F",
                "Francia" = "#DE985D",
                "Países Bajos" = "#D68F5C",
                "Argentina" = "#CE875B",
                "Colombia" = "#C77E59",
                "Rumanía" = "#BF7558",
                "Chile" = "#B76D57",
                "China" = "#B06455",
                "Cuba" = "#A85C54",
                "Italia" = "#A05353",
                "Noruega" = "#994A51",
                "Uruguay" = "#914250",
                "Bolivia" = "#89394F",
                "Dinamarca" = "#82304D",
                "Irlanda" = "#7A284C",
                "República Dominicana" = "#721F4B",
                "Suiza" = "#6B174A"
                )
        ) +
        geom_label(
                aes(label = n, y = n, fill = pais_nacimiento),
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

ggsave("script_figuras/figuras_def/fig5balt.png", width = 12, height = 10, dpi = 300, bg = "transparent")

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
fig8 <- df %>%
    mutate(prestacion=if_else(!is.na(fecha_prestacion), 1, 0)) %>%
    filter(prestacion==1) %>%
    select(ccaa, edad)
t <- df %>%
    mutate(prestacion = if_else(!is.na(fecha_prestacion), 1, 0)) %>%
    filter(prestacion == 1)

fig8 <- as_tibble(tigerstats::rowPerc(xtabs(~ccaa + edad, fig8))) %>%
    filter(edad != "Total")
t <- as_tibble(tigerstats::rowPerc(xtabs(~ edad, t))) %>%
    pivot_longer(1:8) %>%
    filter(name != "Total") %>%
    rename(edad = name, n = value) %>%
    mutate(ccaa = "Total")
fig8 <- bind_rows(fig8, t)

ggplot(fig8, aes(x = n, y = ccaa, fill = factor(edad, levels = rev(c("<30", "30-39", "40-49", "50-59", "60-69", "70-79", ">80"))))) +
    geom_bar(stat = "identity", position = "stack") + 
    theme_classic() +
    labs(x = element_blank(), y = element_blank(), fill = "Tramo de edad") +
    scale_fill_manual(
        values = c(
            "<30" = "#FDBB63",
            "30-39" = "#E49F5E",
            "40-49" = "#CC845A",
            "50-59" = "#B46956",
            "60-69" = "#9B4D52",
            "70-79" = "#83324E",
            ">80" = "#6B174A"
        )
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position = "bottom", axis.text.y = element_text(size = 14)) +
    labs(title = "Prestaciones por CC. AA. y tramo de edad 2024") +
    theme(
        plot.title = element_text(size = 20, margin = margin(b = 20)),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal"
    ) +
    guides(fill = guide_legend(ncol = 7, nrow = 1, byrow = TRUE, reverse = TRUE)) +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(t = 10, r = 30, b = 60, l = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(.4, -.035) # desplaza la leyenda hacia la izquierda
    ) +
    scale_y_discrete(
        expand = c(0, 0),
        labels = c(
            "Andalucía" = "Andalucía",
            "Aragón" = "Aragón",
            "Asturias" = "Asturias",
            "Canarias" = "Canarias",
            "Cantabria" = "Cantabria",
            "Castilla y León" = "Castilla y León",
            "Castilla-La Mancha" = "Castilla-La Mancha",
            "Cataluña" = "Cataluña",
            "Comunidad Valenciana" = "Com. Valenciana",
            "Extremadura" = "Extremadura",
            "Galicia" = "Galicia",
            "Islas Baleares" = "Islas Baleares",
            "Madrid" = "Madrid",
            "Murcia" = "Murcia",
            "Navarra" = "Navarra",
            "País Vasco" = "País Vasco",
            "La Rioja" = "La Rioja",
            "Ceuta" = "Ceuta",
            "Melilla" = "Melilla",
            "Total" = expression(bold("TOTAL"))
        )
    )

ggsave("script_figuras/figuras_def/fig8.png", width = 10, height = 12, dpi = 300, bg = "transparent")

# (AP. 5.3.) Figura 9: Prestaciones por lugar de prestación y CC. AA. 2024 (barras apiladas por CC. AA.)
fig9 <- df %>%
    filter(lugar_prestacion!="") %>%
    group_by(ccaa, lugar_prestacion) %>%
    summarise(n = n())
t9 <- df %>%
    filter(lugar_prestacion!="") %>%
    group_by(lugar_prestacion) %>%
    summarise(n = n()) %>%
    mutate(ccaa = "Total")
fig9 <- as_tibble(tigerstats::rowPerc(xtabs(~ccaa + lugar_prestacion, fig9))) %>%
    filter(lugar_prestacion != "Total")
t9 <- as_tibble(tigerstats::rowPerc(xtabs(~ lugar_prestacion, t9))) %>%
    pivot_longer(1:4) %>%
    filter(name != "Total") %>%
    rename(lugar_prestacion = name, n = value) %>%
    mutate(ccaa = "Total")
fig9 <- bind_rows(fig9, t9)

ggplot(fig9, aes(x=n, y=ccaa, fill=lugar_prestacion)) +
    geom_bar(stat="identity", position="stack") +
    theme_classic() +
    labs(x=element_blank(), y=element_blank(), fill="Lugar de prestación") +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual(
        values = c(
            "Centro Hospitalario" = "#FDBB63",
            "Centro No Sanitario" = "#CC845A",
            "Centro Sociosanitario" = "#9B4D52",
            "Domicilio" = "#6B174A"
        )) +
theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(t = 20, r = 30, b = 80, l = 10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(.4, -.045) # desplaza la leyenda hacia la izquierda
    ) +
    labs(title = "Prestaciones por lugar de prestación y CC. AA. 2024") +
    theme(plot.title = element_text(size = 20, margin = margin(b = 20))) +
    theme(axis.text.y = element_text(size = 14)) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE, title.theme = element_text(size = 18), label.theme = element_text(size = 16))) +
    scale_y_discrete(
        expand = c(0, 0),
        labels = c(
            "Andalucía" = "Andalucía",
            "Aragón" = "Aragón",
            "Asturias" = "Asturias",
            "Canarias" = "Canarias",
            "Cantabria" = "Cantabria",
            "Castilla y León" = "Castilla y León",
            "Castilla-La Mancha" = "Castilla-La Mancha",
            "Cataluña" = "Cataluña",
            "Comunidad Valenciana" = "Com. Valenciana",
            "Extremadura" = "Extremadura",
            "Galicia" = "Galicia",
            "Islas Baleares" = "Islas Baleares",
            "Madrid" = "Madrid",
            "Murcia" = "Murcia",
            "Navarra" = "Navarra",
            "País Vasco" = "País Vasco",
            "La Rioja" = "La Rioja",
            "Ceuta" = "Ceuta",
            "Melilla" = "Melilla",
            "Total" = expression(bold("TOTAL"))
        )
    )

ggsave("script_figuras/figuras_def/fig9.png", width = 10, height = 12, dpi = 300, bg = "transparent")

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
        plot.title = element_text(size = 20, margin = margin(l = -10)),
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

# (AP. 6) Figura 11: Mapa de denegaciones por CC. AA. (puntos)
fig11 <- df %>%
    filter(denegado == 1) %>%
    group_by(ccaa) %>%
    summarise(n = n()) 
fig11 <- full_join(fig11, mapCCAA, by = "ccaa") %>%
    mutate(n = if_else(ccaa=="Ceuta" | ccaa=="Melilla", 0, n))
fig11_centroids <- st_centroid(fig11$geometry)
fig11_points <- fig11 %>%
  mutate(centroid = fig11_centroids) %>%
  st_as_sf()

ggplot(fig11) +
    geom_sf(aes(geometry = geometry, fill = n), color = "black") +
    geom_sf(data = fig11_points, aes(geometry = centroid, size = n), color = "#6b174a", alpha = .8) +
    scale_size_continuous(
        range = c(0, 16),
        name = "Denegaciones"
    ) +
    scale_fill_gradientn(
        colors = colors_gradient,
        name = "Denegaciones"
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
        title = "Denegaciones por CC. AA. 2024",
        fill = "Nº Denegaciones"
    ) +
    geom_sf(aes(geometry = geometry), fill = NA, color = "black", size = 0.5)

ggsave("script_figuras/figuras_def/fig11.png", width = 10, height = 8, dpi = 300, bg = "transparent")

# (AP. 7.1) Figura 12: Mapa de fallecimientos durante tramitación por CC. AA. (puntos)
fig12 <- df %>%
    filter(fallecimiento_tramitacion == 1) %>%
    group_by(ccaa) %>%
    summarise(n = n()) 
fig12 <- full_join(fig12, mapCCAA, by = "ccaa") %>%
    mutate(n = if_else(ccaa=="Ceuta" | ccaa=="Melilla", 0, n))
fig12_centroids <- st_centroid(fig12$geometry)
fig12_points <- fig12 %>%
  mutate(centroid = fig12_centroids) %>%
  st_as_sf()

ggplot(fig12) +
    geom_sf(aes(geometry = geometry, fill = log2(n)), color = "black") +
    geom_sf(data = fig12_points, aes(geometry = centroid, size = n^(.7)), color = "#6b174a", alpha = .8) +
    scale_size_continuous(
        range = c(0, 16),
        name = "Fallecimientos durante tramitación",
        labels = function(x) round(6*x, 1)
    ) +
    scale_fill_gradientn(
        colors = colors_gradient,
        name = "Fallecimientos durante tramitación",
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
        legend.background = element_rect(fill = "transparent", color = NA),
    ) +
    labs(
        title = "Fallecimientos durante tramitación por CC. AA. 2024",
        fill = "Nº Fallecimientos durante tramitación"
    ) +
    geom_sf(aes(geometry = geometry), fill = NA, color = "black", size = 0.5)

ggsave("script_figuras/figuras_def/fig12.png", width = 10, height = 8, dpi = 300, bg = "transparent")

# (AP. 7.2) Figura 13: Fallecimientos durante tramitación por CC. AA. y tramo de edad 2024 (barras apiladas por CC. AA.)
fig13 <- df %>%
    filter(fallecimiento_tramitacion == 1) %>%
    select(ccaa, edad)
t13 <- df %>%
    filter(fallecimiento_tramitacion == 1) %>%
    select(edad) %>%
    mutate(ccaa = "Total")
fig13 <- as_tibble(tigerstats::rowPerc(xtabs(~ccaa + edad, fig13))) %>%
    filter(edad != "Total")
t13 <- as_tibble(tigerstats::rowPerc(xtabs(~ edad, t13))) %>%
    pivot_longer(1:7) %>%
    filter(name != "Total") %>%
    rename(edad = name, n = value) %>%
    mutate(ccaa = "Total")
fig13 <- bind_rows(fig13, t13)

ggplot(fig13, aes(x = n, y = ccaa, fill = factor(edad, levels = rev(c("<30", "30-39", "40-49", "50-59", "60-69", "70-79", ">80"))))) +
    geom_bar(stat = "identity", position = "stack") + 
    theme_classic() +
    labs(x = element_blank(), y = element_blank(), fill = "Tramo de edad") +
    scale_fill_manual(
        values = c(
            "<30" = "#FDBB63",
            "30-39" = "#E49F5E",
            "40-49" = "#CC845A",
            "50-59" = "#B46956",
            "60-69" = "#9B4D52",
            "70-79" = "#83324E",
            ">80" = "#6B174A"
        )
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position = "bottom", axis.text.y = element_text(size = 14)) +
    labs(title = "Fallecimientos durante tramitación por CC. AA. y tramo de edad 2024") +
    theme(
        plot.title = element_text(size = 20, margin = margin(b = 20, l = -80)),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal"
    ) +
    guides(fill = guide_legend(ncol = 7, nrow = 1, byrow = TRUE, reverse = TRUE)) +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(t=10, r=50, b=60, l=10),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.background=element_rect(fill="transparent", color=NA),
        legend.background=element_rect(fill="transparent", color=NA),
        legend.position=c(.4, -.035) # desplaza la leyenda hacia la izquierda
    ) +
    scale_y_discrete(
        expand=c(0,0),
        labels=c(
            "Andalucía" = "Andalucía",
            "Aragón" = "Aragón",
            "Asturias" = "Asturias",
            "Canarias" = "Canarias",
            "Cantabria" = "Cantabria",
            "Castilla y León" = "Castilla y León",
            "Castilla-La Mancha" = "Castilla-La Mancha",
            "Cataluña" = "Cataluña",
            "Comunidad Valenciana" = "Comunidad Valenciana",
            "Extremadura" = "Extremadura",
            "Galicia" = "Galicia",
            "Islas Baleares" = "Islas Baleares",
            "Madrid" = "Madrid",
            "Murcia" = "Murcia",
            "Navarra" = "Navarra",
            "País Vasco" = "País Vasco",
            "La Rioja" = "La Rioja",
            "Ceuta" = "Ceuta",
            "Melilla" = "Melilla",
            "Total" = expression(bold("TOTAL"))
        )
    )

ggsave("script_figuras/figuras_def/fig13.png", width = 10, height = 12, dpi = 300, bg = "transparent")

# (AP. 8.1) Figura 14: Mapa de revocaciones por CC. AA. (puntos)
fig14 <- df %>%
    filter(revocacion == 1) %>%
    group_by(ccaa) %>%
    summarise(n = n())
fig14 <- full_join(fig14, mapCCAA, by = "ccaa") %>%
    mutate(n = if_else(is.na(n), 0, n))

fig14_centroids <- st_centroid(fig14$geometry)
fig14_points <- fig14 %>%
  mutate(centroid = fig14_centroids) %>%
  st_as_sf()

ggplot(fig14) +
    geom_sf(aes(geometry = geometry, fill = n), color = "black") +
    geom_sf(data = fig14_points, aes(geometry = centroid, size = n), color = "#6b174a", alpha = .8) +
    scale_size_continuous(
        range = c(0, 14),
        name = "Revocaciones"
    ) +
    scale_fill_gradientn(
        colors = colors_gradient,
        name = "Revocaciones"
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
        title = "Revocaciones por CC. AA. 2024",
        fill = "Nº Revocaciones"
    ) +
    geom_sf(aes(geometry=geometry), fill=NA, color="black", size=0.5)

ggsave("script_figuras/figuras_def/fig14.png", width = 10, height = 8, dpi = 300, bg = "transparent")

# (AP. 9.1) Figura 15: Especialidad del MR y MC 2024 (barras)
order <- c("Medicina de Familia", "Oncología", "Neurología", "Medicina Interna", "Geriatría", "Cuidados Paliativos", "Neumología", "Otra")
df %>%
    mutate(especialidad_mr = if_else(especialidad_mr == "cuidados paliativos", "Cuidados Paliativos", especialidad_mr)) %>%
    filter(especialidad_mr != "") %>%
ggplot(., aes(x=factor(especialidad_mr, levels=order))) +
    geom_bar(aes(fill = factor(especialidad_mr, levels=order))) +
    theme_classic() +
    labs(x=element_blank(), y=element_blank()) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, 630)) +
    theme(
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 10, r = 30, b = 10, l = 90)
    ) +
    scale_fill_manual(
        values = c(
            "Medicina de Familia" = "#FDBB63",
            "Oncología" = "#E8A35F",
            "Neurología" = "#D38C5B",
            "Medicina Interna" = "#BE7458",
            "Geriatría" = "#A95D54",
            "Cuidados Paliativos" = "#944551",
            "Neumología" = "#7F2E4D",
            "Otra" = "#6B174A"
        )
        ) +
        labs(title = "Especialidad del médico/a responsable 2024") +
        geom_label(
            stat = "count",
            aes(label = after_stat(count), y = after_stat(count), fill = factor(especialidad_mr, levels = order)),
            color = "black",
            alpha = 0.5,
            size = 7,
            label.size = 0.5,
            show.legend = FALSE,
            vjust = -0.4
        ) +
        theme(
            plot.title = element_text(size = 24, margin = margin(b = 20)),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA)
        )

ggsave("script_figuras/figuras_def/fig15a.png", width = 10, height = 12, dpi = 300, bg = "transparent")

df %>%
    filter(especialidad_mc != "") %>%
ggplot(., aes(x=factor(especialidad_mc, levels=order))) +
    geom_bar(aes(fill = factor(especialidad_mc, levels=order))) +
    theme_classic() +
    labs(x=element_blank(), y=element_blank()) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, 175)) +
    theme(
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 10, r = 30, b = 10, l = 90)
    ) +
    scale_fill_manual(
        values = c(
            "Medicina de Familia" = "#FDBB63",
            "Oncología" = "#E8A35F",
            "Neurología" = "#D38C5B",
            "Medicina Interna" = "#BE7458",
            "Geriatría" = "#A95D54",
            "Cuidados Paliativos" = "#944551",
            "Neumología" = "#7F2E4D",
            "Otra" = "#6B174A"
        )
        ) +
        labs(title = "Especialidad del médico/a consultor/a 2024") +
        geom_label(
            stat = "count",
            aes(label = after_stat(count), y = after_stat(count), fill = factor(especialidad_mc, levels = order)),
            color = "black",
            alpha = 0.5,
            size = 7,
            label.size = 0.5,
            show.legend = FALSE,
            vjust = -0.4
        ) +
        theme(
            plot.title = element_text(size = 24, margin = margin(b = 20)),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA)
        )

ggsave("script_figuras/figuras_def/fig15b.png", width = 10, height = 12, dpi = 300, bg = "transparent")

# (AP. 9.2) Figura 16: Histórico 2021-2024 de media de plazos en cada fase (barras apiladas, eje X=media de días)
fig16 <- tibble::tibble(
   LORE = c(
    "1ª sol. - Prestación",
    "1ª sol. - 2ª sol.",
    "2ª sol. - Informe MC",
    "Informe MC - Resolución CGyE",
    "Resolución CGyE - Prestación",
    "Reclamación - Resolución CGyE"
  ),
  m2021 = c(60, 21, 13, 13, 25, 19),
  m2022 = c(75, 26, 13, 12, 23, 23),
  m2023 = c(67, 22, 12.26, 14.70, 20.26, 24.25),
  m2024 = c(82.60, 21.70, 17.50, 15.10, 27.40, 25.50)
)

fig16_long <- fig16 %>%
  pivot_longer(cols = starts_with("m"), names_to = "year", values_to = "days") %>%
  mutate(year = factor(year, levels = c("m2021", "m2022", "m2023", "m2024")),
    year = case_when(
        year == "m2021" ~ "2021",
        year == "m2022" ~ "2022",
        year == "m2023" ~ "2023",
        year == "m2024" ~ "2024"
    ))
fig16_longT <- fig16_long %>% filter(LORE == "1ª sol. - Prestación")
fig16_longR <- fig16_long %>% filter(LORE != "1ª sol. - Prestación")

order = rev(c(
    "1ª sol. - 2ª sol.",
    "2ª sol. - Informe MC",
    "Informe MC - Resolución CGyE",
    "Resolución CGyE - Prestación",
    "Reclamación - Resolución CGyE"
))

ggplot() +
    geom_bar(
        data = fig16_longT,
        aes(y = days, x = year, fill = LORE),
        stat = "identity",
        position = "identity",
        width = 0.4,
        just = 1
    ) +
    geom_label(
        data = fig16_longT,
        aes(y = days, x = year, label = round(days, 0), fill = LORE),
        color = "black",
        fill = "white",
        size = 5,
        label.size = 0.6,
        show.legend = FALSE,
        vjust = 1.5, # etiqueta centrada horizontalmente
        hjust = 2.5 # etiqueta dentro de la barra, cerca del borde superior
    ) +
    geom_bar(
        data = fig16_longR,
        aes(y = days, x = year, fill = factor(LORE, levels = order)),
        stat = "identity",
        position = "stack",
        width = 0.4,
        just = 0
    ) +
    geom_label(
        data = fig16_longR,
        aes(y = days, x = year, label = round(days, 0), fill = LORE),
        color = "black",
        fill = "white",
        position = "stack",
        size = 5,
        label.size = 0.6,
        show.legend = FALSE,
        vjust = 1.5, # etiqueta centrada horizontalmente
        hjust = -1.7 # etiqueta dentro de la barra, cerca del borde superior
    ) +
    theme_classic() +
    labs(y = "Media de días", x = element_blank(), fill = "Fase") +
    scale_fill_manual(
        values = c(
            "1ª sol. - Prestación" = "#FDBB63",
            "1ª sol. - 2ª sol." = "#DF9A5E",
            "2ª sol. - Informe MC" = "#C27959",
            "Informe MC - Resolución CGyE" = "#A55854",
            "Resolución CGyE - Prestación" = "#88374F",
            "Reclamación - Resolución CGyE" = "#6B174A"
        )
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
    theme(
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(t = 10, r = 30, b = 10, l = 70),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA)
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = "Histórico 2021-2024 de media de plazos en cada fase") +
    theme(
        plot.title = element_text(size = 20, margin = margin(b = 20)),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20)
    )

ggsave("script_figuras/figuras_def/fig16horizontal.png", width = 16, height = 14, dpi = 300, bg = "transparent")
