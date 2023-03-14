library(shiny)
library(leaflet)
library(tidyverse)
library(shinythemes)
library(htmltools)
library(RColorBrewer)

# THERE ARE A FEW DATASETS:

# -LADS by ONS: purely geographic 
geo_LAD <- read_csv("Local_Authority_Districts_(December_2022)_Boundaries_UK_BFE.csv")

# -titles_by_lad: count of titles within a LAD
lad_titles <- read_delim("titles_by_lad.csv", 
                    delim = ",", escape_double = FALSE,  
                    locale = locale(), trim_ws = TRUE) %>% 
  rename("Local Authority District" = LAD,
         "Number of titles" = n) 

# -directory_with_hyperlocals: directory that needs to be cleaned, purely needed to extract info like ownership, and frequency from
titles_info <- readRDS("directory_with_hyperlocals.RDS")

# - directory_geo_final, where each title is matched to a LAD
directory <- readRDS("directory_geo_final.rds")

# - twitter info
twitter <- readRDS("twitter_handles.RDS")


##### STEP ONE: increment amount of information available for each outlet
directory1 <- directory %>% 
  left_join(titles_info, by = "Publication") %>% 
  select(Publication, publisher_recoded, Postcode, LAD, id, Website, Cost, Launch, `Date of closure/launch/change`, Frequency) %>% 
  left_join(twitter, by = "Publication") %>% 
  group_by(Publication) %>% 
  slice_head(n = 1) %>% 
  mutate(Launch = if_else(is.na(`Date of closure/launch/change`), Launch, `Date of closure/launch/change`)) %>% 
  select(-`Date of closure/launch/change`) %>% 
  rename("Owner" = publisher_recoded,
         "Twitter handle" = twitter_handle) %>% 
  ungroup() %>% 
  group_by(LAD) %>% 
  mutate("Total number of titles in the same Local Authority District" = n())

map_data <- geo_LAD %>% 
  left_join(lad_titles, by = c("LAD22NM" = "Local Authority District")) %>% 
  rename(Latitude = "LAT", 
         Longitude = "LONG") %>% 
  mutate(`Number of titles` = replace_na(`Number of titles`, 0)) 


# Choices for drop-downs
vars <- c(
    "Number.of.titles" = "Number.of.titles")


# polygons
library(geojsonio)
map <- geojson_read("Local_Authority_Districts_(December_2022)_Boundaries_UK_BUC.geojson", what = "sp")
merged_data <- merge(map, map_data, by = "LAD22NM")
merged_geojson <- geojson_json(merged_data, lat = "Latitude", lon = "Longitude")
geojson_write(merged_geojson, file = "merged_shapefile.shp")

map_data <- geojson_read("merged_shapefile.shp", what = "sp")

############################### UI  ###############################################################

# here is where the user interface architecture is built
      
      # the navbar
ui <- navbarPage("Local print and digital news providers in the United Kingdom in March 2023", id="nav", theme = shinytheme("paper"),
           # the interactive map
           tabPanel("Interactive map",
                    div(class="outer",
                        leafletOutput("mymap", height = 600),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = FALSE, top = 150, left = "auto", right = 20, bottom = "auto",
                                     width = 330, height = "auto",
                                     h3("Customise your map"),
                                     selectInput("color", "  Color", vars),
                        ),
                        tags$div(id="cite",
                                 'Please note that the map only provides information relating to specific local authority districts.
                                 To see information regarding existing titles, head over to the Data Explorer. Data compiled by Simona Bisiani (2023).'
                        )
                    )
           ),
           
           # # the data table explorer
           # tabPanel(
           #     "Data Explorer", div(class ="outer"),
           #     
           #     # with the different filter options
           #     fluidRow(
           #       column(3,
           #              selectInput("Number of titles",
           #                          "Number of titles:",
           #                          c("All",
           #                            unique(as.character(map_data$`Number of titles`))))
           #       ),
           #     ),
           #     # Create a new row for the table.
           #     DT::dataTableOutput("ex1")
           # ),
           
)


############################ SERVER #######################################################

server <- function(input, output, session) {
    
pal <- colorFactor("Accent", map_data$`Number.of.titles`)
  
     output$mymap <- renderLeaflet({
        map_data %>% 
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
         addPolygons(
           fillColor = ~pal(Number.of.titles),
           weight = 0.2,
           opacity = 1,
           color = "grey",
           dashArray = "0",
           fillOpacity = 0.7) %>% 
            # addCircleMarkers(~Longitude, ~Latitude, layerId=~LAD22NM,
            #                                   stroke= FALSE, opacity= 0.7, color=pal, radius = 6.5,
            #                                   fillColor = pal,
            #                                   fillOpacity = 0.7) %>% 
        setView(lng = -3.2765753, lat = 54.7023545, zoom = 6)
     })

    # Modify marker colours depending on user input
    observe({
      colorBy <- input$color
      if (colorBy == "Number.of.titles") {
        colorData <- map_data[[colorBy]]
        pal <- colorFactor("Set2", colorData)
      } 
      
      
     
      leafletProxy("mymap", data = map_data) %>%
        
        clearShapes() %>%
        
        addPolygons(
          fillColor = ~pal(Number.of.titles),
          weight = 0.2,
          opacity = 1,
          color = "grey",
          dashArray = "0",
          fillOpacity = 0.7) %>% 
        
        # addCircleMarkers(~Longitude, ~Latitude, layerId=~LAD22NM,
        #                  stroke= FALSE, opacity= 0.7, color=pal(colorData), radius = 6.5,
        #                  fillColor = pal(colorData),
        #                  fillOpacity = 0.7,
        #                  #label = ~sprintf("<strong>%s</strong><br/>%s<br/>Start: %s<br/>End: %s</br>Type: %s</br>Mode: %s</br>Cost: %s</br>City: %s</br> ", Event, Organiser, Start, End, Type, Mode, Cost, City) %>% lapply(htmltools::HTML),
        #                  labelOptions = labelOptions(
        #                    style = list("font-weight" = "normal", padding = "3px 8px"),
        #                    textsize = "11px", direction = "auto")) %>% 
        
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy, opacity = 1,
                  layerId="colorLegend")
    })
    
    

# # here is where the filter options for the table appear    
# 
#     output$ex1 <- DT::renderDataTable(DT::datatable({
#         j_table <- j_table %>% 
#           mutate(URL = paste0("<a href='", URL,"' target='_blank'>", URL,"</a>"))
#         
#         
#         if (input$Topic != "All") {
#           j_table <- j_table[j_table$Topic == input$Topic,]
#         }
#         if (input$Organiser != "All") {
#             j_table <- j_table[j_table$Organiser == input$Organiser,]
#         }
#         if (input$Mode != "All") {
#             j_table <- j_table[j_table$Mode == input$Mode,]
#         }
#         if (input$Type != "All") {
#             j_table <- j_table[j_table$Type == input$Type,]
#         }
#         if (input$Cost != "All") {
#           j_table <- j_table[j_table$Cost == input$Cost,]
#         }
#         
#         
#         
#         j_table
#     }, escape = FALSE, rownames = FALSE))
} 



######################### RUN ###########################################################

shinyApp(ui, server)


