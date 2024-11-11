############################################
## Evolving Maps with evolMap             ##
############################################
library(evolMap)
library(sf)

## Pompeii map example form a geojson file
# Source: https://github.com/scriptorivm/pompeii
# Read geojson
domi <- st_read('https://raw.githubusercontent.com/scriptorivm/pompeii/master/geojson/domi.geojson')

# Format information for the description of each parcel
domi[["info"]] <- paste0("<iframe src=\"",sub("http:","https:",domi[["N3"]]),"\"></iframe>")
domi[is.na(domi[["N3"]]),"info"] <- "Missing info"

# Map creation and add information of geojson
map <- create_map(center=c(40.750556,14.489722), zoom=16)
map <- add_entities(map,domi,info="info")
plot(map,directory = "pompeii")

# Example of changing the Map provider
# https://leaflet-extras.github.io/leaflet-providers/preview/
map <- create_map(center=c(40.750556,14.489722), zoom=16, provider="OpenStreetMap.HOT")
map <- add_entities(map,domi,info="info")
plot(map,directory = "pompeii_provider")

## Ukraine war Example
# Source: https://ukraine.bellingcat.com/
library(jsonlite)

data <- read.csv("ukr-civharm-2023-02-27.csv")
data[["date"]] <- as.Date(data[["date"]],"%m/%d/%Y")

data[["type"]] <- NA
for(i in seq_len(nrow(data))){
  if(data[i,"associations"]!=""){
    data[i,"type"] <- unlist(strsplit(unlist(strsplit(data[i,"associations"],","))[1],"="))[2]
  }
}

map <- create_map(center=c(49.3402,31.9146),zoom=6.75)
map <- add_markers(map, data, color = "type",
  latitude = "latitude", longitude = "longitude",
  text = "description", start = "date")
plot(map, dir="ukraine")


# Example of changing the Map style with de mode = 2 parameter
map <- create_map(center=c(49.3402,31.9146),zoom=6.75, mode=2)
map <- add_markers(map, data, color = "type",
  latitude = "latitude", longitude = "longitude",
  text = "description", start = "date")
plot(map, dir="ukraineNew")