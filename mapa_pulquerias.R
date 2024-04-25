library(sf)
library(tidyverse)
library(leaflet)

my_sf <- read_sf("~/INEGI_DENUE_20042024.shp")

#me creo una variable extra como identificador
my_sf$ID_MUN <- paste(my_sf$cve_ent, my_sf$cve_mun, sep = '.')


par(mar = c(0, 0, 0, 0))
#Miro los puntos en la retícula
ggplot(my_sf) +
  geom_sf() 

#Se hace el vector de nombres para tener las zonas geográficas

alcaldias <- c("Azcapotzalco",
                              "Coyoacán",
                 "Cuajimalpa de Morelos",
                     "Gustavo A. Madero",
                             "Iztacalco",
                            "Iztapalapa",
                "La Magdalena Contreras",
                            "Milpa Alta",
                        "Álvaro Obregón",
                               "Tláhuac",
                               "Tlalpan",
                            "Xochimilco",
                         "Benito Juárez",
                            "Cuauhtémoc",
                        "Miguel Hidalgo",
                   "Venustiano Carranza")
pa_centroide <- st_read("~/CDMX_mpal.geojson") 

ctrd  <- st_centroid(pa_centroide) 

ctrd <- ctrd[,c("NOM_MUN", "CVE_MUN", "CVE_ENT",   "geometry")]
#Se crea el identificador para hacer el merge abajo
ctrd$ID_MUN <- paste(ctrd$CVE_ENT, ctrd$CVE_MUN, sep = '.')

#Se carga el shape de la CDMX
ruta <- "~/CDMX_mpal.geojson"
mapa_a <- st_read(ruta) 

aux <- as.data.frame(my_sf[,c("ID_MUN", "nom_estab")])
#Aquí se pegan las bases usando el identificador que definimos
a <- left_join(ctrd, aux, by = "ID_MUN") %>% filter(!is.na(nom_estab))
a <- a %>%
  mutate(x = st_coordinates(a)[,1],
         y = st_coordinates(a)[,2])

class(a)

#Primera vista de las alcaldías con locales
a %>%
  ggplot() +
  geom_sf(data = pa_centroide) +
  geom_point(aes(x = x, y = y),
             color = "red")

#Distintas vistas del mapa interactivo:
# 1. Este mapa muestra los locales agrupados por alcaldía
leaflet(a) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
   addCircleMarkers(radius = ~2,
                   fillColor = "yellow",
                   fillOpacity = 0.8,
                   opacity = 1,
                   weight = 0.3,
                   color = "red",
                   popup = lapply(paste0("<b>Estado: </b>", a$NOM_MUN, "<br>" ),
                                  htmltools::HTML))

#2. Este mapa pone todos los locales en sus respectivas localidades
leaflet(my_sf) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(radius = 4,
                   fillColor = "yellow",
                   fillOpacity = 0.8,
                   opacity = 1,
                   weight = 0.3,
                   color = "red",
                  
                   )
#3. Mapa con el shape de la CDMX
ruta <- "~/CDMX_mpal.geojson"
mapa_a <- st_read(ruta) 

# Transform to leaflet projection if needed
data_map <- st_transform(mapa_a, crs = '+proj=longlat +datum=WGS84')

leaflet(my_sf) %>%
  addTiles() %>%
    addPolygons(data = data_map, color = "blue", stroke = T, opacity = 0.5, weight=2 ) %>% 
  addCircleMarkers(radius = 4,
                   fillColor = "yellow",
                   fillOpacity = 0.8,
                   opacity = 1,
                   weight = 0.3,
                   color = "red",
                   
  )

