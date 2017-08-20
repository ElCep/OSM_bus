library(leaflet)
library(rgdal)

rm(list = ls())

setwd("/home/delaye/github/Dakar_bus/")

#read datas 
bus.stop <- readOGR("export.geojson", require_geomType="wkbPoint")
bus.line <- readOGR("export.geojson", require_geomType="wkbLineString")

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
## Créer des groupe dans le S4 pour être capabble de les afficher et mask au besoin
bus.stop.spt <- split(bus.stop, bus.stop@data$ref)


m <- leaflet() %>% 
  setView(lng = -17.4688, lat = 14.7115, zoom = 13)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = bus.line, color = "red", weight = 4)
## function to create marker grouped
names(bus.stop.spt) %>%
  purrr::walk( function(df) {
    m <<- m %>%
      addCircleMarkers(
        data=bus.stop.spt[[df]],
        group = df
      )
  })

m %>%
  addLayersControl(
    overlayGroups = names(bus.stop.spt),
    options = layersControlOptions(collapsed = FALSE)
  )
