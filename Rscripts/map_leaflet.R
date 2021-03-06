library(leaflet)
library(rgdal)
library(stringr)

rm(list = ls())

setwd("/home/delaye/github/OSM_bus/")

#read datas 
bus.stop <- readOGR("./Datas/export.geojson", require_geomType="wkbPoint")
bus.line <- readOGR("./Datas/export.geojson", require_geomType="wkbLineString")

## Récupérer le numéro de la ligne
a <- bus.stop@data
a$X.relations <- gsub("\\{|\\}|\\[|\\]","", a$X.relations)
a$X.relations <- gsub('\"', '', a$X.relations)


t.start <- str_locate(a$X.relations, "ref:")[,2] + 1
b <- str_sub(a$X.relations, start = t.start)
t.end <- str_locate(b,",")[,1]
b <- str_sub(b, end = t.end)
bus.stop@data$ref <- gsub(',|[[:space:]]', '', b)
bus.stop@data <- subset(bus.stop@data, select = c("operator","ref")) 

bus.line@data <- subset(bus.line@data, select = c("operator", "name", "ref"))
bus.line@data$ref <- gsub(',|[[:space:]]', '', bus.line@data$ref)


#bus.line <- bus.line[which(!duplicated(as.data.frame(bus.line))), ]



## Créer des groupe dans le S4 pour être capabble de les afficher et mask au besoin
# bus.stop.spt <- split(bus.stop, bus.stop@data$ref)
bus.line.spt <- split(bus.line, bus.line@data$ref)

# ## Les arrêtes sont amovible ####
# m <- leaflet() %>% 
#   setView(lng = -17.4688, lat = 14.7115, zoom = 13)%>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolylines(data = bus.line, color = "red", weight = 4)
# ## function to create marker grouped
# names(bus.stop.spt) %>%
#   purrr::walk( function(df) {
#     m <<- m %>%
#       addCircleMarkers(
#         data=bus.stop.spt[[df]],
#         group = df
#       )
#   })
# 
# m %>%
#   addLayersControl(
#     overlayGroups = names(bus.stop.spt),
#     options = layersControlOptions(collapsed = FALSE)
#   )

## les lignes sont amovible ####
n <- leaflet() %>% 
  setView(lng = -17.4688, lat = 14.7115, zoom = 13)%>%
  addProviderTiles(providers$CartoDB.Positron)

## function to create marker grouped
names(bus.line.spt) %>%
  purrr::walk( function(df) {
    n <<- n %>%
      addPolylines(data = bus.line.spt[[df]], group = df, color = "red", weight = 4)
  })

n %>%
  addLayersControl(
    overlayGroups = names(bus.line.spt),
    options = layersControlOptions(collapsed = FALSE)
  )

