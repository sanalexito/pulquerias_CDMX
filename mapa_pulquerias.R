library(sf)
my_sf <- read_sf("C:/Users/52552/Documents/Alexito/mapa_pulquerias/INEGI_DENUE_20042024.shp")

#me creo una variable extra como identificador
my_sf$ID_MUN <- paste(my_sf$cve_ent, my_sf$cve_mun, sep = '.')


par(mar = c(0, 0, 0, 0))
plot(st_geometry(my_sf), col = "#f2f2f2", bg = "skyblue", lwd = 0.25, border = 0)

library(ggplot2)
ggplot(my_sf) +
  geom_sf() 


library(ggplot2)
ggplot(my_sf) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()

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
pa_centroide <- st_read("C:/Users/52552/OneDrive - INEGI/Respaldo/varios/Medioambiente/banana/geojsons/Division Politica/CDMX_mpal.geojson") 
ruta <- "C:/Users/52552/OneDrive - INEGI/Respaldo/varios/mariz/mapas/leaflet_v2024/"
ctrd  <- st_centroid(pa_centroide) 

ctrd <- ctrd[,c("NOM_MUN", "CVE_MUN", "CVE_ENT",   "geometry")]
ctrd$ID_MUN <- paste(ctrd$CVE_ENT, ctrd$CVE_MUN, sep = '.')



ruta <- "C:/Users/52552/OneDrive - INEGI/Respaldo/varios/Medioambiente/banana/geojsons/Division Politica/CDMX_mpal.geojson"
mapa_a <- st_read(ruta) 

aux <- as.data.frame(my_sf[,c("ID_MUN", "nom_estab")])

a <- left_join(ctrd, aux, by = "ID_MUN") %>% filter(!is.na(nom_estab))
a <- a %>%
  mutate(x = st_coordinates(a)[,1],
         y = st_coordinates(a)[,2])

class(a)


a %>%
  ggplot() +
  geom_sf(data = pa_centroide) +
  geom_point(aes(x = x, y = y),
             color = "red")

m1 <-leaflet(a) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
   addCircleMarkers(radius = ~2,
                   fillColor = "yellow",
                   fillOpacity = 0.8,
                   opacity = 1,
                   weight = 0.3,
                   color = "red",
                   popup = lapply(paste0("<b>Estado: </b>", a$NOM_MUN, "<br>" ),
                                  htmltools::HTML))


m <- leaflet(my_sf) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(radius = 4,
                   fillColor = "yellow",
                   fillOpacity = 0.8,
                   opacity = 1,
                   weight = 0.3,
                   color = "red",
                  
                   )
data_map <- st_transform(mapa, crs = '+proj=longlat +datum=WGS84')


m <- leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(data_map)


ruta <- "C:/Users/52552/OneDrive - INEGI/Respaldo/varios/Medioambiente/banana/geojsons/Division Politica/CDMX_mpal.geojson"
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

