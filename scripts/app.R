library(shiny)
library(leaflet)
library(tidyverse)
library(shinythemes)
library(htmltools)
library(RColorBrewer)

# Load your dataset
observations <- read_csv("directory1.csv")
titles_in_lads <- read_csv("titles_in_lad_list.csv")
counts_for_lads <- read_csv("counts_for_lads.csv")
urls <- read_csv("urls2.csv")


write_delim(titles_in_lads, delim = ";;", "lads.csv")

data <- counts_for_lads %>% 
  ungroup() %>% 
  mutate(`Number of titles` =
           replace_na(`Total number of titles in the same Local Authority District`, 0)) %>% 
  select(LAD, `Number of titles`) %>% 
  left_join(titles_in_lads, by = "LAD") %>% 
  mutate(across(.cols = everything(), ~replace_na(., ""))) %>% 
  arrange(LAD) %>% 
  rename("Local Authority District" = LAD)

data2 <- observations %>% 
  select(Publication, Owner, LAD, indep, Frequency, Cost, Website, `Twitter handle`) %>% 
  group_by(Publication) %>% 
  summarise(
    across(everything(),                                   # apply to all columns
           ~paste0(unique(na.omit(.x)), collapse = "; "))) %>% 
  mutate(Website = str_remove(Website, "; .*")) %>% 
  rename("Type of owner" = indep,
         "Local Authority District" = LAD)

write_delim(data2, delim = ";;",  "observations.csv")

# # polygons
# library(geojsonio) # struggling to install geojsonio on Linux
# map <- geojson_read("Local_Authority_Districts_(December_2022)_Boundaries_UK_BUC.geojson", what = "sp")
# merged_data <- merge(map, map_data, by = "LAD22NM")
# merged_geojson <- geojson_json(merged_data, lat = "Latitude", lon = "Longitude")
# geojson_write(merged_geojson, file = "merged_shapefile.shp")
# map_data <- geojson_read("merged_shapefile.shp", what = "sp")

############################### UI

ui <- navbarPage("Local print and digital news providers in the United Kingdom in March 2023", 
                 id="nav", theme = shinytheme("paper"),

           # the data table explorer
           tabPanel(
               "Database of outlets", div(class ="outer"),
               DT::dataTableOutput("ex1")
           ),
           
           tabPanel(
             "Database of districts", div(class ="outer"),
             DT::dataTableOutput("ex2")
           ),
           
)

############################ SERVER

server <- function(input, output, session) {
        output$ex1 <- DT::renderDataTable(DT::datatable({
        data
    }, escape = FALSE, rownames = FALSE))
    
    
    output$ex2 <- DT::renderDataTable(DT::datatable({
    data2
    }, escape = FALSE, rownames = FALSE))
} 

######################### RUN

shinyApp(ui, server)


