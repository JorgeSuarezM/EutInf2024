setwd("C:/Users/Usuario/Desktop/INEDyTO/Informe Anual Eutanasia/Script")

#Cargar paquetes y librerías necesarias
  #install.packages("mapSpain")
  #install.packages("sf")
  #install.packages("tmap")

library(mapSpain)
library(sf)
library(tmap)
library(dplyr)

# Descargar geometría de CCAA. Esto descargar un objeto 'sf', que es un df espacial. 
#Con resolución 1, normalmente suele bastar.
#Para descargar el objeto que tiene la geometría de las ccaa de España, usamos la función 'esp_get_ccaa'
ccaa <- esp_get_ccaa(res = "1")


# Visualizar con tmap. con 'plot' ponemos el mapa estático. 
# tm_shape define el objeto que vamos a usar (en este caso las ccaa).
#tm_polygons() pinta las regiones y tm_layout() ajusta detalles como el título del mapa.
tmap_mode("plot")
tm_shape(ccaa) +
  tm_polygons() +
  tm_layout(title = "CCAA de España")


# Crear una tabla con datos ficticios por CCAA
datos_ccaa <- data.frame(
  ine_ccaa = c(
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
    "11", "12", "13", "14", "15", "16", "17"
  ),
  comunidad = c(
    "Andalucía", "Aragón", "Asturias", "Baleares", "Canarias", "Cantabria", 
    "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana",
    "Extremadura", "Galicia", "Madrid", "Murcia", "Navarra", "País Vasco", "La Rioja"
  ),
  renta_media = c(
    19500, 22500, 21500, 23000, 21000, 22000, 21000, 20500, 26000, 22000,
    20000, 21500, 29000, 21000, 26500, 27000, 23500
  ),
  paro = c(
    19.5, 11.2, 13.1, 10.5, 18.0, 11.0, 12.2, 14.0, 10.3, 13.5,
    17.8, 13.0, 9.8, 15.3, 9.2, 8.9, 12.0
  )
)

#Unir los datos (datos_ccaa) a la geometría (ccaa) usando left_join() de dplyr.
#El objeto ccaa que descargué con esp_get_ccaa() tiene un campo 'codauto' que 
#coincide con ine_ccaa. 'codauto'da un número a cada ccaa: 01=Andalucía... 
#Datos_ccaa (en concreto, ine_ccaa) contiene la misma info, asíq lo igualamos
ccaa_datos <- ccaa %>%
  left_join(datos_ccaa, by = c("codauto" = "ine_ccaa"))

#Esto le está diciendo a R: "Junta los datos de datos_ccaa al mapa ccaa, usando 
#como conexión el codauto (del mapa) y el ine_ccaa (de los datos)."
#Así, el resultado (ccaa_datos) tiene:
#La forma geogdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACgAAAAkCAYAAAD7PHgWAAABBklEQVR4Xu2XMQrCQBBFBQvR6wgJHsEDpHVjBDvvoBhbI3bWCkZbFUyhFrYiEat0WgmC6AVkdQqbIVmWZAOi82C64b+/bDWZDEEQP4phTLMaa9d003bTGMgu1psF7JVGNzuWPdzs18GDz443rgrIcndXbvW8g1axGfZKo7P2eBXc+WB74a3FGXtiA1kwzfnpqTF7hL3SwDfAaz+BqvjkwYADe6WhglQwJlQwKVQwKakVTGOoYNL5z4JxwBlUMEwqAu9SwTCpCLxLBcOkIvCusoKT9/WFQ6OkIvCukoJwt5rO0sehUVIReBem6ng+OLBXmnKjn4PbGM5PeKnqgXIlo5vHXoL4Nl4ZYqbbEGA7+wAAAABJRU5ErkJggg==ráfica de cada comunidad (de ccaa) +
#Los datos de paro y renta (de datos_ccaa)
#Todo en un solo objeto sf, que ya podemos pintar con tmap.


#Visualizar con tmap:
tmap_mode("plot")

# Mapa de la renta media
tm_shape(ccaa_datos) +
  tm_polygons("renta_media", palette = "Blues", title = "Renta media (€)") +
  tm_layout(title = "Renta media por CCAA")

# Mapa del paro
tm_shape(ccaa_datos) +
  tm_polygons("paro", palette = "Reds", title = "Tasa de paro (%)") +
  tm_layout(title = "Tasa de paro por CCAA")
