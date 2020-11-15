#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    library(shiny)
    library(stringr)
    library(dplyr)
    library(leaflet)
    library(sf)
    
    accidentes <- read.csv("www/accidentes_medellin.csv",header = TRUE,encoding='UTF-8')
    accidentes$comuna <- str_replace_all(accidentes$comuna, "laureles-estadio","laureles estadio")
    accidentes$fecha <- as.Date(accidentes$fecha) 
    barrios <- read_sf("barrios/Barrio_Vereda.shp")
    barrios <- mutate_if(barrios, is.character, tolower)
    
    #MAPA DE ANTECEDENTES
    
    #para poder llenar luego los barrios
    filtro <- reactive({
        accidentes %>%
            filter(
                comuna == tolower(input$comuna_mapa),
                fecha >= isolate(input$fecha_mapa[1]),
                fecha <= isolate(input$fecha_mapa[2])
            )
    })


    #crea el input de barrio
    output$select_barrio <- renderUI({
    #barrio
        selectInput(
          "barrio_mapa",
          label = "Barrio",
          selected = "TODOS",
          choices = c("TODOS",toupper(unique(filtro()$barrio)))
            )
    })
    

    #crea el mapa inicial
    output$mapa <- renderLeaflet({
      leaflet() %>%
        setView(lng=-75.56359,lat=6.25184,zoom=12)%>%addProviderTiles("CartoDB.Positron")
    })
    
    #modifica el mapa dependiendo de lo que ingrese el usuario
    observe({
      
      datos_mapa <- filtro()
      if(input$comuna_mapa != "---" && input$clase_mapa != "TODOS"){
        if(input$barrio_mapa == "TODOS"){
          datos_mapa <- filtro()%>%
            filter(
              clase == tolower(input$clase_mapa)
            )
        }else{
          datos_mapa <- filtro()%>%
            filter(
              clase == tolower(input$clase_mapa),
              barrio == tolower(input$barrio_mapa)
            )
        }
        
      }else if(input$comuna_mapa != "---" && input$clase_mapa == "TODOS"){
        if(input$barrio_mapa == "TODOS"){
          datos_mapa <-filtro()
        }else{
          datos_mapa <-filtro()%>%
            filter(
              barrio == tolower(input$barrio_mapa)
            )
        }
 
      }
      
      colores <- colorFactor("RdYlGn",datos_mapa$clase)
      
      if(input$comuna_mapa != "---"){
        leafletProxy("mapa", data = datos_mapa) %>% 
        clearShapes() %>% 
        clearControls() %>% 
          addCircles(
          lng = ~longitud,
          lat = ~latitud,
          color = ~colores(clase),
          radius = 20,
          stroke = FALSE,
          fill = TRUE,
          fillOpacity = .7, 
          )%>% 
        fitBounds(~min(longitud), ~min(latitud), ~max(longitud), ~max(latitud))%>%
        addLegend("bottomright", pal = colores, values = ~clase, title = "Clase") 
      }
    })
    
    #PREDICCIONES
    
      
    mi_df <- data.frame(
      "dia_anno" = NULL,
      "choque" = NULL,
      "otro" = NULL,
      "atropello" = NULL,
      "volcamiento" = NULL,
      "caida ocupante" = NULL,
      "incendio" = NULL,
      "choque y atropello" = NULL
    )
    
    output$prediccion <- DT::renderDataTable(DT::datatable({
      if(input$tipo_prediccion != "---"){
        # load(modelo.Rdata)
        
        dia_anno<-sample(1:365, size = 100, replace = TRUE)
        choque<-sample(1:365, size = 100, replace = TRUE)
        otro<-sample(1:365, size = 100, replace = TRUE)
        atropello<-sample(1:365, size = 100, replace = TRUE)
        volcamiento<-sample(1:365, size = 100, replace = TRUE)
        caida_ocupante<-sample(1:365, size = 100, replace = TRUE)
        incendio<-sample(1:365, size = 100, replace = TRUE)
        coque_y_atropello<-sample(1:365, size = 100, replace = TRUE)
        mi_df <- data.frame(dia_anno,choque,otro,atropello,volcamiento,caida_ocupante,
                            incendio,coque_y_atropello)
      }
      mi_df
    }))

    
    #CLUSTER
    
    
    porBarrio <- distinct(accidentes,comuna)
    # se unen las tablas
    mapaBarrio <- right_join(barrios, porBarrio,
                             by = c("NOMBRE_COM" = "comuna"))
    mapaBarrio <- group_by(mapaBarrio,NOMBRE)
    output$cluster <- renderLeaflet({
      coloresComuna <- colorFactor("RdYlGn", mapaBarrio$NOMBRE_COM)

      leaflet(mapaBarrio) %>%addProviderTiles("CartoDB.Positron")%>%
        addPolygons(
          color = ~coloresComuna(NOMBRE_COM),
          opacity = 0.9,
          weight = 1, # grosor de la linea
          fillOpacity = 0.6,
          label = mapaBarrio$NOMBRE,
          highlightOptions = highlightOptions(color = "white",
                                              weight = 2,
                                              bringToFront = TRUE)

        )


    })
    

})
