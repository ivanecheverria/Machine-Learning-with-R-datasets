library(RCurl)
library(RJSONIO)
library(plyr)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "<a class="vglnk" href="http://maps.google.com/maps/api/geocode/" rel="nofollow"><span>http</span><span>://</span><span>maps</span><span>.</span><span>google</span><span>.</span><span>com</span><span>/</span><span>maps</span><span>/</span><span>api</span><span>/</span><span>geocode</span><span>/</span></a>"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

#Test with a single address
address <- geoCode("The White House, Washington, DC")

#address
#[1] "38.8976831"
#[2] "-77.0364972"
#[3] "APPROXIMATE"
#[4] "The White House, 1600 Pennsylvania Avenue Northwest, Washington, D.C., DC 20500, USA"

# Use plyr to getgeocoding for a vector
#address <- c("The White House, Washington, DC","The Capitol, Washington, DC")
#locations <- ldply(address, function(x) geoCode(x))
#names(locations) <- c("lat","lon","location_type", "forAddress")

#Location type, for more info check here: <a class="vglnk" href="https://developers.google.com/maps/documentation/directions/" rel="nofollow"><span>https</span><span>://</span><span>developers</span><span>.</span><span>google</span><span>.</span><span>com</span><span>/</span><span>maps</span><span>/</span><span>documentation</span><span>/</span><span>directions</span><span>/</span></a>
#"ROOFTOP" indicates that the returned result is a precise geocode for which we have location information accurate down to street address precision.
#RANGE_INTERPOLATED" indicates that the returned result reflects an approximation (usually on a road) interpolated between two precise points (such as intersections). Interpolated results are generally returned when rooftop geocodes are unavailable for a street address.
#GEOMETRIC_CENTER" indicates that the returned result is the geometric center of a result such as a polyline (for example, a street) or polygon (region).
#APPROXIMATE" indicates that the returned result is approximate.

# INSTALL DEPENDENCIES IF YOU HAVEN'T ALREADY DONE SO
require(devtools)
install_github('ramnathv/rCharts@dev')
install_github('ramnathv/rMaps')

library(rMaps)
map = Leaflet$new()
map$setView(c(40.73846, -73.99413), 16)
map$tileLayer(provider = 'Stamen.TonerLite')

mywaypoints = list(c(40.74119, -73.9925), c(40.73573, -73.99302))

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


######################################################################

library(rMaps)
map = Leaflet$new()
map$setView(c(40.73846, -73.99413), 16)
map$tileLayer(provider = 'Stamen.TonerLite')

display_chart = function(viz){
  y = paste(capture.output(viz$show('iframesrc', cdn = TRUE)), collapse = "\n")
  IRdisplay::display_html(y)
}

display_chart(map)

mywaypoints = list(c(40.74119, -73.9925), c(40.73573, -73.99302))

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
display_chart(map)


#####Googleway Package####
google_map(key = "your_api_key") %>%
  add_bicycling()

