# Load your dataset
observations <- read_csv("directory1.csv")
titles_in_lads <- read_csv("titles_in_lad_list.csv")
counts_for_lads <- read_csv("counts_for_lads.csv")

urls <- readRDS("urls.RDs")

urls <- urls %>%
  filter(!is.null(twitter)) %>%
  mutate(
    twitter = unlist(twitter),
    twitter = if_else(str_detect(twitter, "Error"), NA_character_, twitter)
  ) %>%
  filter(!is.na(twitter)) %>%
  mutate(
    twitter_handle = str_remove(twitter, ".*twitter.com/"),
    twitter_handle = str_remove(twitter_handle, "\\?.*"),
    twitter_handle = str_remove(twitter_handle, "\\/.*"),
    twitter_handle = str_remove(twitter_handle, "@")
  ) %>%
  #full_join(twitter_profiles, by = c("Publication", "twitter_handle")) %>%
  #select(twitter_handle, Publication) %>%
  filter(twitter_handle != "459701824769900545") %>%
  mutate(twitter_handle = if_else(Publication == "North Edinburgh Community News", "NorthEdinbNews", twitter_handle)) %>%
  #pivot_wider(names_from = twitter_handle, values_from = Publication) %>%
  #pivot_longer(cols = everything()) %>%
  #rowwise %>%
  #mutate(titles_under_handle = length(unique(value)))
  mutate(across(.cols = everything(), as.character)) %>% 
  mutate(redirect_url = if_else(is.na(redirect_url), Website, redirect_url)) %>% 
  select(-Website)

write_csv(urls, "urls2.csv")






data <- counts_for_lads %>% 
  ungroup() %>% 
  mutate(`Total number of titles in the same Local Authority District` =
           replace_na(`Total number of titles in the same Local Authority District`, 0)) %>% 
  select(LAD, `Total number of titles in the same Local Authority District`) %>% 
  left_join(titles_in_lads, by = "LAD") %>% 
  mutate(path =   str_replace_all(LAD, " ", "_"),
         path =   str_replace_all(path, ",", ""),
         path = str_to_lower(path)) %>% 
  mutate(across(.cols = everything(), ~replace_na(., ""))) %>% 
  arrange(LAD)


# Loop through each observation and generate a Markdown file for it
for (i in seq_along(data$LAD)) {
  # Construct the filename and path for this observation's Markdown file
  filename <- paste0("_observations/", data$path[i], "/index.md") 
  dir.create(dirname(filename), recursive = TRUE)  # Create the directory if it doesn't exist
  
  # Generate the Markdown content for this observation
  markdown_content <- "---
layout: page
title: UK local news database
description: 
img: 
---"
  
  # Write the Markdown content to a file
  writeLines(markdown_content, filename)
}

################################################################################

library(knitr)

data$file_path <- paste(data$path, "/index.md", sep = "")
  
data$LAD <- sprintf("[%s](%s)", data$LAD, data$file_path) 
data <- data %>% select(-c(path, file_path, Publication))

library(kableExtra)
dt <- kbl(data[,1:4], col.names = c("Local Authority District", "Number of titles", "Publication", "Owners"))
dt %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
  scroll_box(width = "100%", height = "600px") %>% 
  save_kable(file = "table1.html", self_contained = T)

saveWidget(table, "table.html")

library(DT)
dt <- datatable(data[,1:4], colnames = c("Local Authority District", "Number of titles", "Publication", "Owners"), 
                options = list(paging = TRUE, ordering = TRUE, searching = TRUE, info = FALSE),           
                callback = JS('table.on("click", "td", function() {
                    $(this).toggleClass("selected");
                });'), elementId = "my-table")
library(htmlwidgets)
saveWidget(dt, "my_table.html")

library(reactable)
data <- iris[10:29, ]
orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ff9500"))(x), maxColorValue = 255)

dt2 <- reactable(
  data,
  columns = list(
    Petal.Length = colDef(style = function(value) {
      normalized <- (value - min(data$Petal.Length)) / (max(data$Petal.Length) - min(data$Petal.Length))
      color <- orange_pal(normalized)
      list(background = color)
    })
  )
)

saveWidget(dt2, "reactable.html")
