
library(shiny)
library(dplyr)
library(sf)
library(leaflet)
require(shinyWidgets)
require(rlang)
require(stringr)
require(RColorBrewer)
require(readxl)
require(DT)

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

Survey_info<-read_excel("Survey_info.xlsx")%>%
  left_join(Sampling%>%
              group_by(Source)%>%
              summarise(across(all_of(unname(Parameters)), ~if_else(sum(.x)>0, "Yes", "")),
                        N_total=sum(N_total), .groups="drop"),
            by=c("Abbreviation"="Source"))%>%
  mutate(across(c(`Data source 1`, `Data source 2`), ~if_else(is.na(.x), "", paste0("<a href='",.x,"'>",Data_source_name,"</a>"))))%>%
  mutate(Survey=if_else(is.na(Survey_link), Survey, paste0("<a href='",Survey_link,"'>",Survey,"</a>")))%>%
  select(-Survey_link, -Data_source_name)%>%
  rename(`Water quality`=Water_quality, `Total sample size`=N_total)%>%
  relocate(`Data source 1`, `Data source 2`, .after = last_col())

# Define UI for application that draws a histogram
ui <- navbarPage("IEP stations map", id="nav",
                 tabPanel("Info", 
                          a(shiny::icon("reply"), "Delta Science shinyapps homepage", href="https://deltascience.shinyapps.io/Home/"),
                          tags$div(tags$h2("Information"), 
                                   tags$p(tags$b("Please contact Sam Bashevkin ", 
                                                 tags$a('(sam.bashevkin@deltacouncil.ca.gov)', 
                                                        href="mailto:sam.bashevkin@deltacouncil.ca.gov?subject=Monitoring%20Shiny%20App"), 
                                                 " at the Delta Science Program with any questions.")),
                                   tags$p(tags$em("Click on the 'Interactive map' tab at the top to view the map of sampling locations.")),
                                   tags$p("This app displays the sampling effort and spatio-temporal coverage of 13 Bay-Delta monitoring programs."), 
                                   tags$p("Sampling effort is based off the latest available data, so some surveys may be missing in recent years for which data have not been released, or for collected data not included in data releases. 
                                          All surveys should be available for 2018 and earlier. 
                                          Sampling locations are approximate and may represent the mean location when multiple locations were available for a station.")),
                          tags$h3("Survey details"),
                          dataTableOutput("Survey_info")
                 ),
                 
                 tabPanel("Interactive map", value="map",
                          
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("mapplot", width="100%", height="100vh"),
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        pickerInput("Parameters", "Parameters of interest (only stations with samples of these parameters will be retained)", choices=Parameters, 
                                                    selected = Parameters, multiple = T, options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
                                        pickerInput("Surveys", "Surveys", choices=Surveys, choicesOpt = list(content = Surveys2),
                                                    selected = Surveys, multiple = T, 
                                                    options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
                                        prettySwitch("Exclude_unfixed", "Exclude unfixed EMP EZ stations?", status = "success", fill = TRUE, bigger=TRUE),
                                        prettySwitch("Years", "Inspect sampling effort for each year?", status = "success", fill = TRUE, bigger=TRUE),
                                        conditionalPanel(condition="input.Years",
                                                         sliderInput("Year", "Select year",
                                                                     min=min(Sampling$Year), max=max(Sampling$Year), value=2018, step=1, sep="",
                                                                     animate=TRUE)),
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
  
  output$Survey_info<-renderDataTable({
    datatable(Survey_info, rownames=F, escape = FALSE, options=list(paging=FALSE))%>%
      formatRound(c('Total sample size'), digits=0, interval = 3, mark = ',')
  })
  
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
  
  pal_effort_rev<-reactive({
    req(input$nav=="map", input$Parameter_legend)
    if(input$Log){
      colorNumeric("viridis", log(Data()[[input$Parameter_legend]]+1), reverse = TRUE)
    }else{
      colorNumeric("viridis", Data()[[input$Parameter_legend]], reverse = TRUE)
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
            addLegend(., "topleft", pal = pal_effort_rev(), values = ~log(data[[input$Parameter_legend]]+1), opacity=1, 
                      labFormat=labelFormat(transform=function(x) sort(round(exp(x)-1), decreasing = TRUE)),
                      title=str_replace(input$Parameter_legend, "_", " "))
        }else{
          addCircleMarkers(., weight = 1, lng = ~Longitude, lat = ~Latitude, 
                           fillColor = ~pal_effort()(data[[input$Parameter_legend]]), color="black", fillOpacity = 0.7, popup=lapply(data$tooltip, htmltools::HTML))%>%
            addLegend(., "topleft", pal = pal_effort_rev(), values = ~data[[input$Parameter_legend]], opacity=1, title=str_replace(input$Parameter_legend, "_", " "), 
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
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
