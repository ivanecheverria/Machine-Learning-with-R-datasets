library(ggmap)

revgeocode(as.numeric(unizar))


mapa <- get_map("Valparaiso, Chile",source = "stamen", maptype = "toner", zoom = 15)
ruta <- route(from = "Calle Molina, Valparaiso", to = "Pedro Montt 1011, Valparaiso")
ggmap(mapa) + 
  geom_path(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
            colour = "red", size = 1.5, data = ruta)

##puntos sobre mapas##
View(crime)
crimes.houston <- subset(crime, ! crime$offense %in% c("auto theft", "theft", "burglary"))
valpoMap <- qmap("valparaiso, Chile", zoom = 14, color = "bw",maptype = "toner", source = "stamen")
HoustonMap <- qmap("valparaiso, Chile", zoom = 14, color = "bw")
HoustonMap +
  geom_point(aes(x = lon, y = lat, colour = offense), data = crimes.houston, size = 1)

HoustonMap +
  geom_point(aes(x = lon, y = lat), data = crimes.houston, size = 1) + 
  facet_wrap(~ offense)


HoustonMap +
  geom_point(aes(x = lon, y = lat), data = crimes.houston, size = 1) + 
  facet_wrap(~ day)

#prueba con datos Mutual#

datos_viña <- viña
valpoMap <- qmap("valparaiso, Chile", zoom = 16, color = "bw",maptype = "toner", source = "stamen")
valpoMap <- qmap("valparaiso, Chile", zoom = 16, color = "bw")
valpoMap +
  geom_point(aes(x = viña$Longitud, y = viña$Latitud, colour = Intermediario), data = datos_viña, size = 1)

valpoMap +
  geom_point(aes(x = viña$Longitud, y = viña$Latitud), data = datos_viña, size = 1) + 
  facet_wrap(~ Intermediario)


HoustonMap +
  geom_point(aes(x = Longitud, y = Latitud), data = crimes.houston, size = 1) + 
  facet_wrap(~ day)


##Rutas sobre mapas##

library(maptools)
# un fichero bajado el Ayto. de Madrid
rutas <- getKMLcoordinates("//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Mapas//130111_vias_ciclistas.kml")

library(plyr)
rutas <- ldply(1:length(rutas), function(x) data.frame(rutas[[x]], id = x))
mapa <- get_map("Madrid", source = "stamen", maptype = "toner", zoom = 12)
ggmap(mapa) + geom_path(aes(x = X1, y = X2, group = id), data = rutas, colour = "red")


##Más allá de los puntos: densidades y retículas##

HoustonMap +
  stat_bin2d(
    aes(x = lon, y = lat, colour = offense, fill = offense),
    size = .5, bins = 30, alpha = 1/2,
    data = crimes.houston
  )

#podemos visualizar intensidades#

HoustonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 size = 2, data = crimes.houston,
                 geom = "polygon"
  )


#Ejemplo de aplicación de rutas#
geocode("Central UB, Barcelona")
revgeocode(c(2.164033, 41.38655), output = "address")
mapdist("Tarragona", "Barcelona")
route("Central UB, Barcelona", "Plaza Catalunya, Barcelona", alternatives = FALSE)

desde <- 'Carrer del Doctor Aiguader 88, Barcelona'
hasta <- 'Plaza Catalunya, Barcelona'

rutas <- route(desde, hasta, alternatives = TRUE)
head(rutas)
ggplot() +
  geom_segment(aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route), size = 1.5, data = rutas)

qmap('Plaza Sant Jaume, Barcelona', zoom = 14,  maptype = "roadmap", extent = "panel") +
  geom_segment(aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route), size = 1.5, data = rutas)

qmap('Plaza Sant Jaume, Barcelona', zoom = 14,  maptype = "roadmap", extent = "panel") +
  geom_segment(aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route), size = 1.5, data = rutas) +
  facet_grid(.~ route)


set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-95.36, 50), amount = .3),
  y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)
map <- get_googlemap('houston', markers = df, path = df, scale = 2)
ggmap(map, extent = 'device')

viña<-read.csv2("//nas//Datos//RespaldoPC//IVECLE01//Api-viña del mar//Api-Viña Del Mar.csv",head=T)
str(viña)


df <- round(data.frame(
  x = jitter(rep(-95.36, 50), amount = .3),
  y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)

df<-round(data.frame(viña$Longitud,viña$Latitud), digits = 2)
map <- get_googlemap("valparaiso,valparaiso, chile", markers = df, path = df, scale = 1)
ggmap(map, extent = 'device')
ggmap(map, extent = "normal")

###the revgeocode###

gc <- geocode("baylor university")
(gc <- as.numeric(gc))
revgeocode(gc)


pkgs<-c("devtools","rgdal","raster","ggmap")
install.packages(pkgs)
library(devtools)
gh_pkgs<-c("rstudio/leaflet","robinlovelace/stplanr")
install_github(gh_pkgs)
lapply(c(pkgs,"leaflet","stplnr"),library, character.only=T)
##rmaps##
require(devtools)
install_github('ramnathv/rCharts@dev')
install_github('ramnathv/rMaps')



library(rMaps)
crosslet(
  x = "country", 
  y = c("web_index", "universal_access", "impact_empowerment", "freedom_openness"),
  data = web_index
)


ichoropleth(Crime ~ State, data = subset(violent_crime, Year == 2010))
ichoropleth(Crime ~ State, data = violent_crime, animate = "Year")
ichoropleth(Crime ~ State, data = violent_crime, animate = "Year", play = TRUE)


map <- Leaflet$new()
map$setView(c(51.505, -0.09), zoom = 13)
map$tileLayer(provider = 'Stamen.Watercolor')
map$marker(
  c(51.5, -0.09),
  bindPopup = 'Hi. I am a popup'
)
map







library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    
    cbind(viña$Longitud,viña$Latitud)
        #cbind(rnorm(45) * 2 - 13, rnorm(110) - 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      
      addMarkers(data = points())  %>%
      # Add a pop up to map by using addPopups() function
      addPopups(lng = viña$Longitud, lat = viña$Latitud, popup = paste(viña$Intermediario, viña$Sucursal,viña$Monto.Deuda,viña$Convenio), 
      options = popupOptions(minWidth = 10, closeOnClick = F, closeButton = T, keepInView = T))
       #mapOptions(zoomToLimits = "first")
    })
  
}

shinyApp(ui, server)

##Prueba Ruta Mapa##

# INSTALL DEPENDENCIES IF YOU HAVEN'T ALREADY DONE SO
# library(devtools)
# install_github("ramnathv/rCharts@dev")
# install_github("ramnathv/rMaps")

library(rMaps)
map = Leaflet$new()
map$setView(c(-33.0507029, -71.6002961), 16)
map$tileLayer(provider = 'Stamen.TonerLite')

mywaypoints = list(c(-33.0388948, -71.3681229), c(-33.0210293, -71.5550649))

map$addAssets(
  css = "http://www.liedman.net/leaflet-routing-machine/dist/leaflet-routing-machine.css",
  jshead = "http://www.liedman.net/leaflet-routing-machine/dist/leaflet-routing-machine.min.js"
)

routingTemplate = "
<script>
var mywaypoints = %s
L.Routing.control({
waypoints: [
L.latLng.apply(null, mywaypoints[0]),
L.latLng.apply(null, mywaypoints[1])
]
}).addTo(map);
</script>"

map$setTemplate(
  afterScript = sprintf(routingTemplate, RJSONIO::toJSON(mywaypoints))
)
map$set(width = 1450, height = 800)
map
