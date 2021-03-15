
library(shiny)
library(dplyr)
library(sf)
library(leaflet)
require(shinyWidgets)
require(rlang)
require(stringr)
require(RColorBrewer)

Sampling<-readRDS("Sampling.Rds")
Survey_names<-tibble(Survey=c('20-mm Survey (20mm)', 'CDFW Bay Study (Baystudy)', 'Delta Juvenile Fish Monitoring Program (DJFMP)', 'Enhanced Delta Smelt Monitoring (EDSM)', 
                              'Environmental Monitoring Program (EMP)', 'Fall Midwater Trawl (FMWT)', 'Fish Restoration Program (FRP)', 'Spring Kodiak Trawl (SKT)', 
                              'Summer Townet (STN)', 'Suisun Marsh Fish Study (Suisun)', 'Bureau of Reclamation Sacramento Ship Channel Surveys (USBR)', 
                              'USGS San Francisco Bay Survey(USGS)', 'Yolo Bypass Fish Monitoring Program (YBFMP)'),
                     Abbreviation=c('20mm', 'Baystudy', 'DJFMP', 'EDSM', 
                                    'EMP', 'FMWT', 'FRP', 'SKT', 
                                    'STN', 'Suisun', 'USBR', 
                                    'USGS', 'YBFMP'))

Parameters<-c("Benthic", "Phytoplankton", "Zooplankton", "Water_quality", "Fish")
names(Parameters)<-str_replace(Parameters, "_", " ")
Surveys<-set_names(Survey_names$Abbreviation, Survey_names$Survey)

Surveys2 <- str_replace_all(str_wrap(names(Surveys), width = 32), "\\n", "<br>")

# Define UI for application that draws a histogram
ui <- navbarPage("IEP stations map", id="nav",
                 tabPanel("Info", 
                          tags$div(tags$h2("Information"), tags$p("This app displays the sampling effort and spatio-temporal coverage of 13 Bay-Delta monitoring programs."), 
                                   tags$p("Sampling effort is based off the latest available data, so some surveys may be missing in recent years for which data have not been released. All surveys should be available for 2018 and earlier."),
                                   tags$p("Sampling locations are approximate."),
                                                   tags$p(tags$b("Please contact Sam Bashevkin ", tags$a('(sam.bashevkin@deltacouncil.ca.gov)', href="mailto:sam.bashevkin@deltacouncil.ca.gov?subject=Monitoring%20Shiny%20App"), " at the Delta Science Program with any questions.")))
                 ),
                 
                 tabPanel("Interactive map", value="map",
                          
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("mapplot", width="100%", height="100vh"),
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        pickerInput("Parameters", "Parameters", choices=Parameters, 
                                                    selected = Parameters, multiple = T, options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
                                        pickerInput("Surveys", "Surveys", choices=Surveys, choicesOpt = list(content = Surveys2),
                                                    selected = Surveys, multiple = T, 
                                                    options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
                                        prettySwitch("Exclude_unfixed", "Exclude unfixed EMP EZ stations?", status = "success", fill = TRUE, bigger=TRUE),
                                        prettySwitch("Years", "Inspect sampling effort for each year?", status = "success", fill = TRUE, bigger=TRUE),
                                        conditionalPanel(condition="input.Years",
                                                         sliderInput("Year", "Select year",
                                                                     min=min(Sampling$Year), max=max(Sampling$Year), value=2018, step=1, sep="")),
                                        radioGroupButtons(
                                          inputId = "Legend",
                                          label = "Variable for color scale", 
                                          choices = c("Survey", "Sampling effort"),
                                          status = "primary"
                                        ),
                                        conditionalPanel(condition="input.Legend=='Sampling effort'",
                                                         uiOutput("Parameter_legend"),
                                                         prettySwitch("Log", "log(x+1) transform scale?", status = "success", fill = TRUE, bigger=TRUE))
                          )
                          
                 ),
                 tags$style(HTML("

.selected {background-color:white !important;}
.dropdown-menu.inner {background-color:#D6D6D6 !important;}
td:first-child {
  font-weight: 900;
}
"))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$Parameters, {
    req(input$nav=="map")
    
    if(is.null(input$Parameters)){
      Parameters<-c("Benthic", "Phytoplankton", "Zooplankton", "Water_quality", "Fish")
    } else{
      Parameters<-input$Parameters
    }
    
    Sources<-Sampling%>%
      filter(if_any(all_of(Parameters), ~.x>0))%>%
      pull(Source)%>%
      unique()
    
    Survey_names<-filter(Survey_names, Abbreviation%in%Sources)
    
    Surveys<-set_names(Survey_names$Abbreviation, Survey_names$Survey)
    
    Surveys2 <- str_replace_all(str_wrap(names(Surveys), width = 32), "\\n", "<br>")
    
    updatePickerInput(session, "Surveys", choices=Surveys, selected=input$Surveys, choicesOpt = list(content = Surveys2))
  })
  
  output$Parameter_legend<-renderUI({
    req(input$nav=="map")
    
    if(nrow(Data())<1){
      Parameters<-c("Benthic", "Phytoplankton", "Zooplankton", "Water_quality", "Fish")
    }else{
      data<-Data()
      
      if(is.null(input$Surveys)){
        Surveys<-unique(data$Source)
      } else{
        Surveys<-input$Surveys
      }
      
      Parameters<-data%>%
        filter(Source%in%Surveys)%>%
        select(Benthic, Phytoplankton, Zooplankton, Water_quality, Fish)%>%
        select(where(~any(.x>0)))%>%
        names()%>%
        sort()
    }
    
    Parameters2<-c(Parameters, "N_total", "N_years")
    
    names(Parameters2)<-c(str_replace(Parameters, "_", " "), "Total", "N_years")
    
    
    
    radioGroupButtons("Parameter_legend", "Which parameter should be used for the sampling effort legend?", choices=Parameters2, 
                selected = if_else(is.null(input$Parameter_legend), "N_total", input$Parameter_legend), status = "primary")
  })
  
  Data<-reactive({
    req(input$nav=="map", input$Surveys, input$Parameters)
    
    if(input$Exclude_unfixed){
      out<-filter(Sampling, !Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"))
    }else{
      out<-Sampling
    }
    
    out<-out%>%
      filter(Source%in%input$Surveys & if_any(all_of(input$Parameters), ~.x>0))
    
    if(input$Years){
      out<-out
    }else{
      out<-out%>%
        group_by(Station, Station2, Extra_stations, Source, Latitude, Longitude, Zoop_station, Benthic_station)%>%
        summarise(across(c(Benthic, Phytoplankton, Zooplankton, Water_quality, Fish, N_total), ~sum(.x)), N_years=unique(N_years), .groups="drop")
    }
    
    return(out)
  })
  
  Data_plot<-reactive({
    req(Data())
    
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
                          "<tr><td>N_Total &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>N_Years &nbsp</td><td>%s</td></tr>")
    }else{
      str_model <- paste0("<tr><td>Survey &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>Station &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>Alternate station names &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>N_Benthic &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>N_Fish &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>N_Phytoplankton &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>N_Water_quality &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>N_Zooplankton &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>N_Total &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>N_Years &nbsp</td><td>%s</td></tr>")
    }
    
    if(input$Years){
      req(input$nav=="map", input$Year)
      Data()%>%
        filter(Year==input$Year)%>%
        mutate(tooltip=sprintf(str_model, Year, Source, Station, if_else(is.na(Extra_stations), "None", Extra_stations), Benthic, Fish, Phytoplankton, Water_quality, Zooplankton, N_total, N_years),
               tooltip=paste0( "<table  border='1'>", tooltip, "</table>" ))
    }else{
      Data()%>%
        mutate(tooltip=sprintf(str_model, Source, Station, if_else(is.na(Extra_stations), "None", Extra_stations), Benthic, Fish, Phytoplankton, Water_quality, Zooplankton, N_total, N_years),
               tooltip=paste0( "<table  border='1'>", tooltip, "</table>" ))
    }
  })
  
    pal_survey<-reactive({
      req(input$nav=="map", Data())
      
      Sources<-unique(Data()$Source)
      
      if(length(Sources)>12){
        colorFactor(colorRampPalette(brewer.pal(12, "Set3"))(length(Sources)), Sources)
      }else{
        colorFactor(brewer.pal(length(Sources), "Set3"), Sources)
      }
         
   })
  
   pal_effort<-reactive({
       req(input$nav=="map", input$Parameter_legend)
     if(input$Log){
       colorNumeric("viridis", log(Data()[[input$Parameter_legend]]+1))
     }else{
       colorNumeric("viridis", Data()[[input$Parameter_legend]])
     }
    })
  
  
  mapplot<-reactive({
    req(input$nav=="map")
    leaflet()%>%
      addProviderTiles("Esri.WorldGrayCanvas")%>%
      fitBounds(min(Sampling$Longitude, na.rm=T), min(Sampling$Latitude), max(Sampling$Longitude), max(Sampling$Latitude))
  })
  
  observe({
    req(input$nav=="map", input$Legend)
    if(input$Legend=="Sampling effort"){
      req(input$Parameter_legend)
    }
    
    data<-Data_plot()
    
    mapplot<-leafletProxy("mapplot", session, data = data)%>%
      clearMarkers() %>%
      clearControls()%>%
      {if(input$Legend=="Sampling effort"){
        {if(input$Log){
          addCircleMarkers(., weight = 1, lng = ~Longitude, lat = ~Latitude, 
                           fillColor = ~pal_effort()(log(data[[input$Parameter_legend]]+1)), color="black", fillOpacity = 0.7, popup=lapply(data$tooltip, htmltools::HTML))%>%
          addLegend(., "topleft", pal = pal_effort(), values = ~log(data[[input$Parameter_legend]]+1), opacity=1, labFormat=labelFormat(transform=function(x) round(exp(x)-1)),
                    title=str_replace(input$Parameter_legend, "_", " "))
          }else{
            addCircleMarkers(., weight = 1, lng = ~Longitude, lat = ~Latitude, 
                             fillColor = ~pal_effort()(data[[input$Parameter_legend]]), color="black", fillOpacity = 0.7, popup=lapply(data$tooltip, htmltools::HTML))%>%
            addLegend(., "topleft", pal = pal_effort(), values = ~data[[input$Parameter_legend]], opacity=1, title=str_replace(input$Parameter_legend, "_", " "))
          }}
          
      }else{
        addCircleMarkers(., weight = 1, lng = ~Longitude, lat = ~Latitude,
                         fillColor = ~pal_survey()(data$Source), color="black", fillOpacity = 0.7, popup=lapply(data$tooltip, htmltools::HTML))%>%
          addLegend(., "topleft", pal = pal_survey(), values = ~data$Source, opacity=1, title="Survey")
      }}
      
  })
  
  output$mapplot <- renderLeaflet({
    mapplot()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
