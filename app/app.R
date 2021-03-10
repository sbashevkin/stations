
library(shiny)
library(dplyr)
library(sf)
library(leaflet)
require(shinyWidgets)
require(rlang)
require(stringr)

Sampling<-readRDS("Sampling.Rds")

# Define UI for application that draws a histogram
ui <- navbarPage("IEP stations map", id="nav",
                 tabPanel("Info", 
                          "..."
                 ),
                 
                 tabPanel("Interactive map", value="map",
                          
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("mapplot", width="100%", height="100vh"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            uiOutput("Surveys"),
                                            uiOutput("Parameters"),
                                            prettySwitch("Exclude_unfixed", "Exclude unfixed EMP EZ stations?", status = "success", fill = TRUE, bigger=TRUE),
                                            prettySwitch("Years", "Inspect sampling effort for each year?", status = "success", fill = TRUE, bigger=TRUE),
                                            conditionalPanel(condition="input.Years",
                                                             sliderInput("Year", "Select year",
                                                                         min=min(Sampling$Year), max=max(Sampling$Year), value=2017, step=1, sep="")),
                                            radioGroupButtons(
                                                inputId = "Legend",
                                                label = "Variable for color scale", 
                                                choices = c("Survey", "Sampling effort"),
                                                status = "primary"
                                            )
                                            )
                          
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$Parameters<-renderUI({
        rep(input$nav=="map")
        if(is.null(input$Surveys)){
            Surveys<-unique(Sampling$Source)
        } else{
            Surveys<-input$Surveys
        }
        Parameters<-Sampling%>%
            filter(Source%in%Surveys)%>%
            select(Benthic, Phytoplankton, Zooplankton, Water_quality, Fish)%>%
            select(where(~any(.x>0)))%>%
            names()%>%
            sort()
        names(Parameters)<-str_replace(Parameters, "_", " ")
        pickerInput("Parameters", "Parameters", choices=Parameters, 
                    selected = Parameters, multiple = T, options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3"))
    })
    
    output$Surveys<-renderUI({
        rep(input$nav=="map")
        if(is.null(input$Parameters)){
            Parameters<-c("Benthic", "Phytoplankton", "Zooplankton", "Water_quality", "Fish")
        } else{
            Parameters<-input$Parameters
        }
        Surveys<-Sampling%>%
            filter(if_any(all_of(Parameters), ~.x>0))%>%
            pull(Source)%>%
            unique()%>%
            sort()
        pickerInput("Surveys", "Surveys", choices=Surveys, 
                    selected = Surveys, multiple = T, options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3"))
    })
    
    Data<-reactive({
        req(input$nav=="map")
        
        if(input$Exclude_unfixed){
            out<-filter(Sampling, !Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"))
        }else{
            out<-Sampling
        }
        
        if(input$Years){
            str_model <- paste0("<tr><td>Year &nbsp</td><td>%s</td></tr>",
                                "<tr><td>Survey &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>Station &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>Alternate station names &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Benthic &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Fish &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Phytoplankton &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Water_quality &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Zooplankton &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Total &nbsp</td><td>%s</td></tr>")
        }else{
            str_model <- paste0("<tr><td>Survey &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>Station &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>Alternate station names &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Benthic &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Fish &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Phytoplankton &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Water_quality &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Zooplankton &nbsp</td><td>%s</td></tr>", 
                                "<tr><td>N_Total &nbsp</td><td>%s</td></tr>")
        }

        
        if(input$Years){
            req(input$Year)
            out%>%
                filter(Year==input$Year & Source%in%input$Surveys & if_any(all_of(input$Parameters), ~.x>0))%>%
                mutate(tooltip=sprintf(str_model, Year, Source, Station, if_else(is.na(Extra_stations), "None", Extra_stations), Benthic, Fish, Phytoplankton, Water_quality, Zooplankton, N_total),
                       tooltip=paste0( "<table>", tooltip, "</table>" ))
        }else{
            out%>%
                group_by(Station, Station2, Extra_stations, Source, Latitude, Longitude, Zoop_station, Benthic_station)%>%
                summarise(across(c(Benthic, Phytoplankton, Zooplankton, Water_quality, Fish, N_total), ~sum(.x)), .groups="drop")%>%
                filter(Source%in%input$Surveys & if_any(all_of(input$Parameters), ~.x>0))%>%
                mutate(tooltip=sprintf(str_model, Source, Station, if_else(is.na(Extra_stations), "None", Extra_stations), Benthic, Fish, Phytoplankton, Water_quality, Zooplankton, N_total),
                       tooltip=paste0( "<table>", tooltip, "</table>" ))
        }
        
    })
    
  #  pal_survey<-reactive({
 #       colorFactor(brewer.pal(7, "Dark2"), mapdatataxa()%>%pull(Taxlifestage))
   # })
    
   # pal_effort<-reactive({
   #     
   # })
    
    
    mapplot<-reactive({
        req(input$nav=="map")
        leaflet()%>%
            addProviderTiles("Esri.WorldGrayCanvas")%>%
            fitBounds(min(Sampling$Longitude, na.rm=T), min(Sampling$Latitude), max(Sampling$Longitude), max(Sampling$Latitude))
    })
    
    observe({
        req(input$nav=="map")
        #if(input$Legend=="Survey"){
        #    pal<-pal_survey()
        #}else{
        #    pal<-pal_effort()
        #}
        mapplot<-leafletProxy("mapplot", session, data = Data())%>%
            clearMarkers() %>%
            addCircleMarkers(weight = 1, lng = ~Longitude, lat = ~Latitude,
                             fillColor = "yellow", color="black", fillOpacity = 0.7, popup=lapply(Data()$tooltip, htmltools::HTML))
    })

    output$mapplot <- renderLeaflet({
        mapplot()
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
