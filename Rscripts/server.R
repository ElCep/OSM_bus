library(shiny)
library(leaflet)
library(rgdal)
library(stringr)



server <- function(input, output) {
 
  output$mymap <- renderLeaflet({
    
    #read datas 
    #bus.stop <- readOGR("./Datas/export.geojson", require_geomType="wkbPoint")
    bus.line <- readOGR("../Datas/export.geojson", require_geomType="wkbLineString")
    
    
    # prepare line 
    bus.line@data <- subset(bus.line@data, select = c("operator", "name", "ref"))
    bus.line@data$ref <- gsub(',|[[:space:]]', '', bus.line@data$ref)
    
    
    ## Créer des groupe dans le S4 pour être capabble de les afficher et mask au besoin
    bus.line.spt <- split(bus.line, bus.line@data$ref)
    
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
    
  })
}