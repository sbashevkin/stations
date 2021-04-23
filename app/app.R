
library(shiny)
library(dplyr)
library(leaflet)
require(shinyWidgets)
require(rlang)
require(stringr)
require(RColorBrewer)
require(readxl)
require(DT)

# Load initial data

Sampling<-readRDS("Sampling.Rds")
Survey_names<-tibble(Survey=c('20-mm Survey (20mm)', 'CDFW Bay Study (Baystudy)', 'Delta Juvenile Fish Monitoring Program (DJFMP)', 'Enhanced Delta Smelt Monitoring (EDSM)', 
                              'Environmental Monitoring Program (EMP)', 'Fall Midwater Trawl (FMWT)', 'Fish Restoration Program (FRP)', 'Spring Kodiak Trawl (SKT)', 'Smelt Larva Survey',
                              'Summer Townet (STN)', 'Suisun Marsh Fish Study (Suisun)', 'Bureau of Reclamation Sacramento Ship Channel Surveys (SDSCS)', 
                              'SFBS San Francisco Bay Survey (SFBS)', 'Yolo Bypass Fish Monitoring Program (YBFMP)'),
                     Abbreviation=c('20mm', 'Baystudy', 'DJFMP', 'EDSM', 
                                    'EMP', 'FMWT', 'FRP', 'SKT', 'SLS',
                                    'STN', 'Suisun', 'SDSCS', 
                                    'SFBS', 'YBFMP'))

Parameters<-c("Benthic", "Phytoplankton", "Zooplankton", "Water_quality", "Fish")
names(Parameters)<-str_replace(Parameters, "_", " ")
Surveys<-set_names(Survey_names$Abbreviation, Survey_names$Survey)

Surveys2 <- str_replace_all(str_wrap(names(Surveys), width = 32), "\\n", "<br>")

Survey_info<-read_excel("Survey_info.xlsx")%>%
  left_join(Sampling%>%
              group_by(Source)%>%
              summarise(across(all_of(unname(Parameters)), ~if_else(sum(.x)>0, "Yes", "")),
                        `Total years of sampling`=max(N_years),
                        `Average number of samples per year per station`=mean(Max), .groups="drop"),
            by=c("Abbreviation"="Source"))%>%
  mutate(across(c(`Data source 1`, `Data source 2`), ~if_else(is.na(.x), "", paste0("<a href='",.x,"'>",Data_source_name,"</a>"))))%>%
  mutate(Survey=if_else(is.na(Survey_link), Survey, paste0("<a href='",Survey_link,"'>",Survey,"</a>")))%>%
  select(-Survey_link, -Data_source_name)%>%
  rename(`Water quality`=Water_quality)%>%
  relocate(`Data source 1`, `Data source 2`, .after = last_col())

# Define UI for application that draws a histogram
ui <- navbarPage("Bay-Delta monitoring", id="nav",
                 
                 # First tab for general info
                 
                 tabPanel("Info", 
                          
                          # Write introductory text
                          
                          a(shiny::icon("reply"), "Delta Science shinyapps homepage", href="https://deltascience.shinyapps.io/Home/"),
                          tags$div(tags$h2("Information"), 
                                   tags$p(tags$em("Click on the 'Interactive map' tab at the top to view the map of sampling locations.")),
                                   tags$p(tags$b("Please contact Sam Bashevkin ", 
                                                 tags$a('(sam.bashevkin@deltacouncil.ca.gov)', 
                                                        href="mailto:sam.bashevkin@deltacouncil.ca.gov?subject=Monitoring%20Shiny%20App"), 
                                                 " at the Delta Science Program with any questions.")),
                                   tags$p("This app displays the sampling effort and spatio-temporal coverage of 13 Bay-Delta monitoring programs."), 
                                   tags$p("Sampling effort is based off the latest available data, so some surveys may be missing in recent years for which data have not been released, or for collected data not included in data releases. 
                                          All surveys should be available for 2018 and earlier. 
                                          Sampling locations are approximate and may represent the mean location when multiple locations were available for a station."),
                                   tags$p("The Fish Restoration Program is currently only represented by their zooplankton sampling."),
                                   a(shiny::icon("github"), "App code is available here", href="https://github.com/sbashevkin/stations")),
                          tags$h3("Survey details"),
                          
                          # Include table of info
                          
                          dataTableOutput("Survey_info")
                 ),
                 
                 # Second tab for map
                 
                 tabPanel("Interactive map", value="map",
                          
                          # Output map
                    
                          leafletOutput("mapplot", width="100%", height="100vh"),
                          
                          # Include panel with user inputs

                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        pickerInput("Parameters", "Parameters of interest (only stations with samples of these parameters will be retained)", choices=Parameters, 
                                                    selected = Parameters, multiple = T, options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
                                        pickerInput("Surveys", "Surveys", choices=Surveys, choicesOpt = list(content = Surveys2),
                                                    selected = Surveys, multiple = T, 
                                                    options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
                                        prettySwitch("Effort_filter", "Filter by sampling effort?", status = "success", fill = TRUE, bigger=TRUE),
                                        conditionalPanel(condition="input.Effort_filter",
                                                         uiOutput("Effort_filter_parameter"),
                                                         uiOutput("Effort_filter_range")),
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
                 
                 # Add custom css to control appearance
                 
                 tags$style(HTML("

.selected {background-color:white !important;}
.selected {color:black !important;}
.dropdown-menu.inner {background-color:#D6D6D6 !important;}
td:first-child {
  font-weight: 900;
}
"))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Survey info table
  output$Survey_info<-renderDataTable({
    datatable(Survey_info, rownames=F, escape = FALSE, options=list(paging=FALSE))%>%
      formatRound(c('Average number of samples per year per station'), digits=0, interval = 3, mark = ',')
  })
  
  # Update the survey input with just the surveys that sample the user-selected parameters
  
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
  
  # Update the choice of sample effort legend depending on the parameters present in the selected dataset
  
  parameter_choices<-reactive({
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
    
    Parameters2<-c(Parameters, "Max", "N_years")
    
    names(Parameters2)<-c(str_replace(Parameters, "_", " "), "Max N over all parameters", "Number of years")
    return(Parameters2)
  })
  
  output$Parameter_legend<-renderUI({
    req(input$nav=="map", parameter_choices())
    choices<-parameter_choices()
    
    
    radioGroupButtons("Parameter_legend", "Which parameter should be used for the sampling effort legend?", choices=choices, 
                      selected = if_else(is.null(input$Parameter_legend), "Max", input$Parameter_legend), status = "primary")
  })
  
  output$Effort_filter_parameter<-renderUI({
    req(input$nav=="map", parameter_choices())
    choices<-parameter_choices()
    
    
    pickerInput("Effort_filter_parameter", "Which metric of sampling effort would you like to filter by?", choices=choices, 
                multiple = F, options=pickerOptions(actionsBox=TRUE, showTick=TRUE))
  })
  
  output$Effort_filter_range<-renderUI({
    req(input$nav=="map", input$Effort_filter_parameter, Data())
    min<-min(Data()[[input$Effort_filter_parameter]])
    max<-max(Data()[[input$Effort_filter_parameter]])
    
    sliderInput("Effort_filter_range", "Select the range of the above-selected sampling effort metric to retain", min=min, max=max,
                value=c(min, max))
  })
  
  # Create an initial dataset of either 1) sampling effort for each year or 2) sampling effort summed across years, depending on user selection to the "Years" switch
  
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
        summarise(across(c(Benthic, Phytoplankton, Zooplankton, Water_quality, Fish, Max), ~sum(.x)), N_years=unique(N_years), .groups="drop")%>%
        mutate(across(c(Benthic, Phytoplankton, Zooplankton, Water_quality, Fish, Max), ~round(.x/N_years, 2)))
    }
    
    return(out)
  })
  
  # Create the dataset used for plotting, including all the info for the clickable popup and filtered to the chosen year if users are choosing years
  
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
                          "<tr><td>N_Max &nbsp</td><td>%s</td></tr>", 
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
                          "<tr><td>N_Max &nbsp</td><td>%s</td></tr>", 
                          "<tr><td>N_Years &nbsp</td><td>%s</td></tr>")
    }
    
    if(input$Effort_filter){
      req(input$Effort_filter_range)
      data<-filter(Data(), .data[[input$Effort_filter_parameter]]>=min(input$Effort_filter_range) & .data[[input$Effort_filter_parameter]]<=max(input$Effort_filter_range))
    }else{
      data<-Data()
    }
    
    if(input$Years){
      req(input$nav=="map", input$Year)
      data%>%
        filter(Year==input$Year)%>%
        mutate(tooltip=sprintf(str_model, Year, Source, Station, if_else(is.na(Extra_stations), "None", Extra_stations), Benthic, Fish, Phytoplankton, Water_quality, Zooplankton, Max, N_years),
               tooltip=paste0( "<table  border='1'>", tooltip, "</table>" ))
    }else{
      data%>%
        mutate(tooltip=sprintf(str_model, Source, Station, if_else(is.na(Extra_stations), "None", Extra_stations), Benthic, Fish, Phytoplankton, Water_quality, Zooplankton, Max, N_years),
               tooltip=paste0( "<table  border='1'>", tooltip, "</table>" ))
    }
  })
  
  # Create the color palette for monitoring surveys
  
  pal_survey<-reactive({
    req(input$nav=="map", Data())
    
    Sources<-unique(Data()$Source)
    
    if(length(Sources)>12){
      colorFactor(colorRampPalette(brewer.pal(12, "Set3"))(length(Sources)), Sources)
    }else{
      colorFactor(brewer.pal(length(Sources), "Set3"), Sources)
    }
    
  })
  
  # Create the color palette for sampling effort
  
  pal_effort<-reactive({
    req(input$nav=="map", input$Parameter_legend)
    if(input$Log){
      colorNumeric("viridis", log(Data()[[input$Parameter_legend]]+1))
    }else{
      colorNumeric("viridis", Data()[[input$Parameter_legend]])
    }
  })
  
  # Create a reverse color palette (required to make the legend ordered logically)
  
  pal_effort_rev<-reactive({
    req(input$nav=="map", input$Parameter_legend)
    if(input$Log){
      colorNumeric("viridis", log(Data()[[input$Parameter_legend]]+1), reverse = TRUE)
    }else{
      colorNumeric("viridis", Data()[[input$Parameter_legend]], reverse = TRUE)
    }
  })
  
  # Create the base map (these components don't change)
  
  mapplot<-reactive({
    req(input$nav=="map")
    leaflet()%>%
      addProviderTiles("Esri.WorldGrayCanvas")%>%
      fitBounds(min(Sampling$Longitude, na.rm=T), min(Sampling$Latitude), max(Sampling$Longitude), max(Sampling$Latitude))
  })
  
  # Create the reactive components of the map that are responsive to user inputs
  
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
                      title=paste0(str_replace(input$Parameter_legend, "_", " "), "\nsamples per year"))
        }else{
          addCircleMarkers(., weight = 1, lng = ~Longitude, lat = ~Latitude, 
                           fillColor = ~pal_effort()(data[[input$Parameter_legend]]), color="black", fillOpacity = 0.7, popup=lapply(data$tooltip, htmltools::HTML))%>%
            addLegend(., "topleft", pal = pal_effort_rev(), values = ~data[[input$Parameter_legend]], opacity=1, title=paste0(str_replace(input$Parameter_legend, "_", " "), "\nsamples per year"), 
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
        }}
        
      }else{
        addCircleMarkers(., weight = 1, lng = ~Longitude, lat = ~Latitude,
                         fillColor = ~pal_survey()(data$Source), color="black", fillOpacity = 0.7, popup=lapply(data$tooltip, htmltools::HTML))%>%
          addLegend(., "topleft", pal = pal_survey(), values = ~data$Source, opacity=1, title="Survey")
      }}
    
  })
  
  # Output the map
  
  output$mapplot <- renderLeaflet({
    mapplot()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
