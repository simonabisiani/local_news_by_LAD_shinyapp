library(tidyverse)
library(readr)

PINF_data <- read_delim("files/PINF_data.csv",
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# my_data <- read_delim("files/observations.csv",
#                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

# # 1. I have duplicates of the same title because I account for multiple local areas covered, but since they all fit within the same lad I should really just count them one
# my_data_revisited <- my_data %>%
#   mutate(Publication = str_remove(Publication, "\\(.*")) %>%
#   group_by(Publication) %>%
#   slice(1) %>%
#   mutate(Title = str_trim(Publication))

# # on full dataset
# not_pinf <- anti_join(my_data_revisited, PINF_data, by = "Title") # titles in me not in PINF
# not_me <- anti_join(PINF_data, my_data_revisited, by = "Title") # titles in pinf not in me
# common_data <- inner_join(PINF_data, my_data_revisited, by = "Title") # common titles
# radio_tv <- PINF_data |> filter(str_detect(`Media Type`, "TV|Radio"))
# closed <- PINF_data |> filter(str_detect(`Active/Closed`, "Closed"))
#
# # put it together
# not_pinf_simple <- not_pinf %>% ungroup() %>% select(Title)
#
# not_matching <- not_me %>% select(Title) %>%  bind_rows(not_pinf_simple, .id = "id") %>%
#   arrange(Title) %>% mutate(source = if_else(id == 1, "PINF", "Simona")) %>% select(-id) %>%
#   anti_join(radio_tv, by = "Title") %>% anti_join(closed, by = "Title")
#
# for(i in 1:nrow(not_matching)){
#   dups = agrep(not_matching$Title[i], not_matching$Title)
#   if(length(dups) != 1){not_matching$possibleDups[i] <- paste(dups[dups != i],
#                                                     collapse = ',')} else {
#                                                       not_matching$possibleDups[i] <- NA
#                                                     }
# }
#
# print(not_matching)
#
# library(openxlsx)
#
# #define sheet names for each data frame
# dataset_names <- list('All_missing' = not_matching, 'Missing_in_PINF' = not_pinf, 'Missing_in_Simona' = not_me)
#
# #export each data frame to separate sheets in same Excel file
# openxlsx::write.xlsx(dataset_names, file = 'PINF_Simona_inconsistencies.xlsx')


###

# FOLLOWING A MANUAL REVIEW OF ALL INCONSISTENCIES BETWEEN ME AND PINF, LET'S FIX MY DIRECTORY
simona_errors <-
  read_csv("files/PINF_Simona_inconsistencies - Missing_in_PINF.csv")
pinf_integrations <-
  read_csv("files/PINF_Simona_inconsistencies - Missing_in_Simona.csv")
my_directory <-
  read_csv("files/Local news UK database - Titles.csv")

radio_tv <- PINF_data |> filter(str_detect(`Media Type`, "TV|Radio"))
closed <- PINF_data |> filter(str_detect(`Active/Closed`, "Closed"))

# cleanup PINF
remove_from_PINF <- pinf_integrations |> 
  filter(Action =="N") 

PINF_clean <- 
  PINF_data |>
  anti_join(radio_tv, by = "Title") |> 
  anti_join(closed, by = "Title") |> 
  anti_join(remove_from_PINF, by = "Title")

pinf_to_add_modify <- pinf_integrations |> 
  filter(Action %in% c("A", "M"))

# cleanup me
remove_simona <- simona_errors |> 
  filter(str_detect(...12, paste0(c("losed", "IPL", "remove", "art of"), collapse = "|")))

my_directory_clean <- my_directory |> 
  anti_join(remove_simona, by = "Publication")

# merger 
me_pinf <- full_join(my_directory_clean, pinf_to_add_modify, by = c("Publication" = "Title")) |> left_join(PINF_data, by = c("Publication" = "Title"))

# put it together
not_me <- anti_join(PINF_clean, me_pinf, by = c("Title" = "Publication"))
not_pinf <- anti_join(me_pinf, PINF_clean, by = c("Publication" = "Title"))

# my directory final polishing (fix owner, remove redundant columns, ...)
me_final <- me_pinf |> 
  mutate(Owner = if_else(is.na(Owner.x), Owner.y, Owner.x)) |> 
  mutate(Owner = if_else(Owner == "Presumed independent", Owner.y, Owner)) |> 
  mutate(Owner = if_else(is.na(Owner), "Presumed independent", Owner)) |> 
  filter(`Active/Closed` == "Active"|is.na(`Active/Closed`)) |> 
  mutate(Website = if_else(str_detect(Website, "tag"), URL.y, Website),
         Website = if_else(is.na(Website), URL.x, Website),
         Website = if_else(is.na(Website), URL.y, Website),
         Twitter = if_else(is.na(Twitter), `Twitter handle`, Twitter)) |> 
  select(-c(Owner.x, Owner.y, `Active/Closed`, Comments.x, Comments.y, Action, `Add / Add but modify / not add - comments`, URL.x, URL.y, `Twitter handle`))

write_csv(me_final, "directory_after_pinf_merge.csv")

# after importing this on google sheets I fixed various small inconsistencies in publisher names, nas, frequency spellings, etc. So please find the updated sheet here: https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit?usp=sharing

