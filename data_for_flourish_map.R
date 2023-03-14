# THERE ARE A FEW DATASETS:
library(tidyverse)

# geo official data
geo_LAD <- read_csv("Local_Authority_Districts_(December_2022)_Boundaries_UK_BFE.csv")

#################################################################################

# STEP ONE: a dataset where each row is a LAD, and each column a summary statistics of properties of titles in that LAD

lad_titles <- read_delim("titles_by_lad.csv",
                         delim = ",", escape_double = FALSE,
                         locale = locale(), trim_ws = TRUE) %>%
  rename("Local Authority District" = LAD,
         "Number of titles" = n) %>%
  right_join(geo_LAD, by = c("Local Authority District" = "LAD22NM")) %>%
  mutate(`Number of titles` = replace_na(`Number of titles`, 0))

# directory_with_hyperlocals: directory that needs to be cleaned, purely needed to extract info like ownership, and frequency from
titles_info <- readRDS("directory_with_hyperlocals.RDS")

# - directory_geo_final, where each title is matched to a LAD
directory <- readRDS("directory_geo_final.rds") %>% 
  group_by(Publication) %>% 
  slice(1) 

# - twitter info
twitter <- readRDS("twitter_handles.RDS")

# final directory
directory1 <- directory %>% 
  left_join(titles_info, by = "Publication") %>% 
  select(Publication, publisher_recoded, Postcode, LAD, id, Website, Cost, Launch, `Date of closure/launch/change`, Frequency) %>% 
  left_join(twitter, by = "Publication") %>% 
  # group_by(Publication) %>% 
  # slice_head(n = 1) %>% 
  mutate(Launch = if_else(is.na(`Date of closure/launch/change`), Launch, `Date of closure/launch/change`)) %>% 
  select(-`Date of closure/launch/change`) %>% 
  rename("Owner" = publisher_recoded,
         "Twitter handle" = twitter_handle) %>% 
  ungroup() %>% 
  group_by(LAD) %>% 
  mutate("Total number of titles in the same Local Authority District" = n()) %>% 
  mutate(Owner = replace_na(Owner, "Presumed independent")) %>%  
  mutate(LAD = if_else(str_detect(Publication, "Salford"), "Salford", LAD), # add LAD to two observations which were missing it
         LAD = if_else(str_detect(Publication, "Shetland News"), "Shetland Islands", LAD)) %>% 
  filter(!is.na(LAD)) %>% # remove guernsey and jersey
  left_join(geo_LAD, by = c("LAD" = "LAD22NM")) %>% 
  mutate(indep = if_else(str_detect(Owner, "independent"), "Independent", "Legacy")) #%>% 
  filter(!is.na(indep))

write_csv(directory1, "directory1.csv")

counts_for_lads <- directory1 %>% 
  ungroup() %>%
  pivot_wider(id_cols = "LAD",
              names_from = "Owner",
              values_from = "Owner",
              values_fn = length) %>% 
  left_join(directory1, by = c("LAD")) %>% 
  relocate(LAD, `Total number of titles in the same Local Authority District`) %>% 
  mutate(across(where(is.numeric), function(x) tidyr::replace_na(x, 0))) %>% 
  group_by(LAD) %>% 
  slice_head(n = 1) %>% 
  right_join(geo_LAD, by = c("LAD" = "LAD22NM"))
  
write_csv(counts_for_lads, "counts_for_lads.csv")

# this for summary popups.... but let's wait for a sec
library(purrr)
df <- map2(colnames(counts_for_lads), counts_for_lads, paste, sep = ': ') %>% 
  list_cbind() 

titles_in_lad_list <- directory1 %>% 
  select(LAD, Publication, Owner) %>% 
  group_by(LAD) %>% 
  summarise(
    across(everything(),                                   # apply to all columns
           ~paste0(unique(na.omit(.x)), collapse = "; "))) # function is defined which combines unique non-NA values

write_csv(titles_in_lad_list, "titles_in_lad_list.csv")
