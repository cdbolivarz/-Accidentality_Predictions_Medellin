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
    library(tidymodels)
    library(ranger)
    library(doParallel)
    library(Metrics)
    
    accidentes <- read.csv("www/accidentes_medellin.csv",header = TRUE,encoding='UTF-8')
    accidentes$comuna <- str_replace_all(accidentes$comuna, "laureles-estadio","laureles estadio")
    accidentes$fecha <- as.Date(accidentes$fecha) 
    barrios <- read_sf("barrios/Barrio_Vereda.shp")
    barrios <- mutate_if(barrios, is.character, tolower)
    datos_prediccion <- read.csv("www/datos_prediccion.csv",header = TRUE,encoding='UTF-8')
    datos_prediccion$fecha <- as.Date(datos_prediccion$fecha)
    datos_prediccion$festivo <- as.logical(datos_prediccion$festivo)
    datos_prediccion <- select(datos_prediccion,fecha,festivo,comuna)
    datos_prediccion <- arrange(datos_prediccion,fecha)
    
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
      "fecha" = NULL,
      "comuna" = NULL,
      "numero_accidentes" = NULL
    )
    
    output$prediccion <- DT::renderDataTable(({
      
      if(input$tipo_prediccion != "---"){
        load("www/modelo_predictivo.Rdata")
        
        predicciones_futuras <- predict(
          modelo, 
          data = datos_prediccion
        )
        
        predicciones_2019_2022 <- datos_prediccion
        predicciones_2019_2022$numero_accidentes <- floor(predicciones_futuras$predictions)
        
        mi_df <- predicciones_2019_2022 %>%
                  filter(
                    fecha >= input$fecha_prediccion[1],
                    fecha <= input$fecha_prediccion[2]
                  )
        
        if(input$tipo_prediccion == "Mensual"){
          mi_df <- mi_df %>%
                  mutate(mes = format(fecha, "%m"), anno = format(fecha, "%Y")) %>%
                  group_by(mes, anno, comuna) %>%summarise(total = sum(numero_accidentes))
          mi_df <- arrange(mi_df,anno)
        }else if(input$tipo_prediccion == "Semanal"){
          mi_df<-mi_df %>%
                  mutate(anno = format(fecha, "%Y"), semana = strftime(fecha, format = "%V")) %>%
                  group_by(semana, anno, comuna) %>%summarise(total = sum(numero_accidentes))
          mi_df <- arrange(mi_df,anno)
        }
      }
      mi_df
    }))

    
    #CLUSTER
  
    load("www/cluster.Rdata")
    barrio <- sort(distinct(accidentes,barrio)$barrio)
    accidentes_vars <- data.frame(barrio)
    accidentes_vars$grupo <- km.res$cluster
    barrios <- right_join(barrios, distinct(accidentes,comuna),
                          by = c("NOMBRE_COM" = "comuna"))
    mapaBarrio <- right_join(barrios, accidentes_vars,
                             by = c("NOMBRE" = "barrio"))
    
    output$cluster <- renderLeaflet({
      coloresBarrio <- colorFactor("YlGnBu", mapaBarrio$grupo)

      leaflet(mapaBarrio) %>%addProviderTiles("CartoDB.Positron")%>%
        addPolygons(
          color = ~coloresBarrio(grupo),
          opacity = 1,
          weight = 1, # grosor de la linea
          fillOpacity = 0.7,
          label = mapaBarrio$NOMBRE,
          highlightOptions = highlightOptions(color = "white",
                                              weight = 2,
                                              bringToFront = TRUE)

        )%>%
        addLegend("bottomright", 
                  pal = coloresBarrio,
                  values = ~grupo,
                  title = "Grupo") 


    })
    

})
