library(shiny)
library(leaflet)
library(tidyverse)
library(shinythemes)
library(htmltools)
library(RColorBrewer)

# Load your dataset
observations <-
  read_csv("files/Local news UK database - Titles.csv")

############################### DATA

data <- observations |>
  mutate(title_owner = paste0(Publication, " (", Owner, ")")) |> 
  separate_rows(`coverage LAD`, sep = "; ") |>
  group_by(`coverage LAD`) |>
  mutate(
    tot_coverage_LAD = n(),
    tot_owner_coverage_LAD = length(unique(Owner))
  ) |>
  ungroup() |>
  mutate(`coverage LAD` = if_else(
    is.na(`coverage LAD`),
    "coverage location unknown",
    `coverage LAD`
  )) |>
  select(
    c(
      title_owner,
      tot_coverage_LAD,
      tot_owner_coverage_LAD,
      `coverage LAD`
    )
  ) |>
  group_by(`coverage LAD`) |>
  mutate(title_owner = paste0(unique(na.omit(title_owner)), collapse = " - ")) |>
  ungroup() |>
  distinct() |>
  mutate(
    `Number of titles` =
      replace_na(tot_coverage_LAD, 0),
    `Number of publishers` = replace_na(tot_owner_coverage_LAD, 0)
  ) |>
  select(`coverage LAD`,
         `Number of titles`,
         `Number of publishers`,
         title_owner) |>
  mutate(across(.cols = everything(), ~ replace_na(., ""))) |>
  arrange(`coverage LAD`) |>
  rename("Local Authority District" = `coverage LAD`,
         "Title (Publisher)" = title_owner) 

data |> 
  write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit#gid=652214043', sheet = "LAD")

# USE THE ABOVE FOR SCATTERPLOT OF LOW HIGH TITLE/PUBLISHERS?

data2 <- observations |>
  rename("Type of owner" = Owner_type) |>
  select(
    c(1:12)
  ) 

############################### UI

ui <-
  navbarPage(
    "Local print and digital news providers in the United Kingdom in April 2023",
    id = "nav",
    theme = shinytheme("paper"),
    
    # the data table explorer
    tabPanel(
      "Database of districts",
      div(class = "outer"),
      DT::dataTableOutput("ex1")
    ),
    
    tabPanel(
      "Database of outlets",
      div(class = "outer"),
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
