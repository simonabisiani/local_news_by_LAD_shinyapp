library(shiny)
library(leaflet)
library(tidyverse)
library(shinythemes)
library(htmltools)
library(RColorBrewer)



j_events <- read_delim("journalism_events.csv", 
                       delim = ";", escape_double = FALSE,  
                       locale = locale(), trim_ws = TRUE)

j_events <- j_events %>% 
  rename("Organiser" = node,
         "Event" = event,
         "Organiser coordinates" = node_loc_coord,
         "City" = ev_loc_city,
         "Country" = ev_loc_country,
         "Type" = ev_type,
         "Cost" = ev_cost,
         "URL" = ev_url,
         "Start" = start_date,
         "End" = end_date,
         "Mode" = mode,
         "Topic" = type) %>% 
  separate(ev_loc_coord, c("Latitude", "Longitude"), sep = ", ") %>% 
  mutate("Latitude" = as.numeric(Latitude),
         "Longitude" = as.numeric(Longitude),
         "Cost" = ifelse(is.na(Cost), "not known", Cost),
         "Organiser" = ifelse(is.na(Organiser), "not known", Organiser)) %>% 
  select(-c(`Organiser coordinates`, node_type)) 
    

write.csv(j_events, "j_event.csv")


j_table <- j_events %>% 
    select(Event, Start, End, Organiser, City, Country, URL, Cost, Type, Mode, Topic) %>% 
    mutate(Cost = ifelse(is.na(Cost), "not known", Cost),
           Organiser = ifelse(is.na(Organiser), "not known", Organiser))

# Choices for drop-downs
vars <- c(
    "Type of event" = "Type",
    "Mode" = "Mode",
    "Cost" = "Cost",
    "Topic" = "Topic"
)






############################### UI  ###############################################################

# here is where the user interface architecture is built
      
      # the navbar
ui <- navbarPage("Journalism Events, Awards, and Conferences in late 2021 to 2022/23", id="nav", theme = shinytheme("paper"),
           # the interactive map
           tabPanel("Interactive map",
                    div(class="outer",
                        leafletOutput("mymap", height = 600),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = TRUE, top = 150, left = "auto", right = 20, bottom = "auto",
                                     width = 330, height = "auto",
                                     h3("Customise your map"),
                                     selectInput("color", "  Color", vars),
                        ),
                        tags$div(id="cite",
                                 'Please note that online events are displayed by locating the organiser. 
                                 Also not all events are displayed on the map, due not having event or organiser location. 
                                 Head over to the data explorer to see all events. Data compiled for', tags$em('Computational Social Journalism'), ' by Simona Bisiani (2021).'
                        )
                    )
           ),
           
           # the data table explorer
           tabPanel(
               "Data Explorer", div(class ="outer"),
               
               # with the different filter options
               fluidRow(
                 column(3,
                        selectInput("Topic",
                                    "Topic:",
                                    c("All",
                                      unique(as.character(j_table$Topic))))
                 ),
                 column(3,
                          selectInput("Organiser",
                                      "Organiser:",
                                      c("All",
                                        unique(as.character(j_table$Organiser))))
                   ),
                   column(3,
                          selectInput("Mode",
                                      "Mode:",
                                      c("All",
                                        unique(as.character(j_table$Mode))))
                   ),
                   column(3,
                          selectInput("Type",
                                      "Type:",
                                      c("All",
                                        unique(as.character(j_table$Type))))
                   ),
                   column(3,
                          selectInput("Cost",
                                      "Cost:",
                                      c("All",
                                        unique(as.character(j_table$Cost))))
                   )
               ),
               # Create a new row for the table.
               DT::dataTableOutput("ex1")
           ),
           
)


############################ SERVER #######################################################

server <- function(input, output, session) {
    
pal <- colorFactor("Accent", j_events$Mode)
  
     output$mymap <- renderLeaflet({
        j_events %>% 
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
            addCircleMarkers(~Longitude, ~Latitude, layerId=~Event,
                                              stroke= FALSE, opacity= 0.7, color=pal, radius = 6.5,
                                              fillColor = pal,
                                              fillOpacity = 0.7) %>% 
            setView(lng = 2.50, lat = 30.45, zoom = 2.5) 
    })

    # Modify marker colours depending on user input
    observe({
      colorBy <- input$color
      if (colorBy == "Topic") {
        colorData <- j_events[[colorBy]]
        pal <- colorFactor("Set2", colorData)
      } 
      if (colorBy == "Mode") {
        colorData <- j_events[[colorBy]]
        pal <- colorFactor("Set2", colorData)
      } 
      if (colorBy == "Type") {
        colorData <- j_events[[colorBy]]
        pal <- colorFactor("Set1", colorData)
      }
      if (colorBy == "Cost") {
        colorData <- j_events[[colorBy]]
        pal <- colorFactor("Dark2", colorData)
      }
      
     
      leafletProxy("mymap", data = j_events) %>%
        clearShapes() %>%
        addCircleMarkers(~Longitude, ~Latitude, layerId=~Event,
                         stroke= FALSE, opacity= 0.7, color=pal(colorData), radius = 6.5,
                         fillColor = pal(colorData),
                         fillOpacity = 0.7,
                         label = ~sprintf("<strong>%s</strong><br/>%s<br/>Start: %s<br/>End: %s</br>Type: %s</br>Mode: %s</br>Cost: %s</br>City: %s</br> ", Event, Organiser, Start, End, Type, Mode, Cost, City) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "11px", direction = "auto")) %>% 
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy, opacity = 1,
                  layerId="colorLegend")
    })
    
    

# here is where the filter options for the table appear    

    output$ex1 <- DT::renderDataTable(DT::datatable({
        j_table <- j_table %>% 
          mutate(URL = paste0("<a href='", URL,"' target='_blank'>", URL,"</a>"))
        
        
        if (input$Topic != "All") {
          j_table <- j_table[j_table$Topic == input$Topic,]
        }
        if (input$Organiser != "All") {
            j_table <- j_table[j_table$Organiser == input$Organiser,]
        }
        if (input$Mode != "All") {
            j_table <- j_table[j_table$Mode == input$Mode,]
        }
        if (input$Type != "All") {
            j_table <- j_table[j_table$Type == input$Type,]
        }
        if (input$Cost != "All") {
          j_table <- j_table[j_table$Cost == input$Cost,]
        }
        
        
        
        j_table
    }, escape = FALSE, rownames = FALSE))
} 



######################### RUN ###########################################################

shinyApp(ui, server)


