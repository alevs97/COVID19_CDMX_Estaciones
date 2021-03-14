library(shiny)
library(leaflet)
library(RColorBrewer)

data1 <- read.csv("../data_sets/Alcaldias_data_set.csv")
data2 <- read.csv("../data_sets/Estaciones_metro.csv")


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Numero de casos", min(data1$casos_totales), max(data1$casos_totales),
                            value = range(data1$casos_totales), step = 0.1
                ),
                selectInput("colors", "Color del esquema",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Rangos IDH", TRUE),
                
                
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    data1[data1$casos_totales>= input$range[1] & data1$casos_totales <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    #(input$colors,data$TOP )
    colorQuantile(input$colors, data1$casos_totales)
  })
  
  
  
  
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data1) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    
    pal <- colorFactor(c("#FF33F0","#3433FF","#BACCA5","#61E5F2","#F5ED0C","#F03514","#F0B014",
                         "#2D9617","#900C3F","#AB4DD7","#B8AEBD","#C3BA11"), domain = c("Linea 1",
                         "Linea 2","Linea 3","Linea 4","Linea 5","Linea 6","Linea 7","Linea 8",
                         "Linea 9","Linea A","Linea B","Linea 12"))
    
    leaflet(data2) %>% addTiles() %>%
      addCircleMarkers(
        radius = 10,
        color = ~pal(estacion),
        stroke = FALSE, fillOpacity = 0.5,
        popup = ~paste(paste("Nombre estacion",nombre_estacion,sep = ":"),
                       paste("Linea del metro",estacion,sep = ":")
                       ,sep="<br/>")
      )
      
  })
  

  observe({
    pal <- colorpal()
    #pal_1 <- colorpal_metro()

    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~sqrt(100)*30, stroke = FALSE,
                 fillColor = ~pal(casos_totales),fillOpacity = 0.8,popup = ~paste(paste("Nombre Colonia",colonia,sep=":"),paste("Total de casos confirmados:",casos_totales,sep=":"),sep="<br/>"))%>%
      setView(lng=-99.12766, lat=19.42847 , zoom=10)

  })
  

  observe({
    proxy <- leafletProxy("map", data = data1)
    
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~data1$casos_totales
      )
    }
  })
  gc()
  memory.size(max=F)
}

shinyApp(ui, server)
