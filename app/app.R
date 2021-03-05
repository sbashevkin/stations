
library(shiny)
library(leaflet)



# Define UI for application that draws a histogram
ui <- navbarPage("IEP stations map", id="nav",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100", height="100"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            ...)
                          )
                 ),
                 tabPanel("Info", 
                          ...
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
        leaflet()%>%
            addProviderTiles("Esri.WorldGrayCanvas")%>%
            fitBounds(~min(Longitude, na.rm=T), ~min(Latitude, na.rm=T), ~max(Longitude, na.rm=T), ~max(Latitude, na.rm=T))
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
