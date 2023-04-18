# 12.04.2023 redoing analysis with updated sheet
library(googlesheets4)
library(tidyverse)
library(PostcodesioR)

# read google sheets data into R
observations <-
  read_sheet(
    'https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit#gid=2004739758'
  )

####################################################

# this section is irrelevant since 17.04 as I have now a "coverage LAD" column in googlesheets that exists for all titles, so the postcode extractions is no longer of any use, as I already have the LAD information I need.
observations <- observations |>
  mutate(
    postcode_c = str_extract(
      `Registered / Company Address`,
      "[A-Z]{1,2}[0-9][A-Z0-9]? [0-9][ABD-HJLNP-UW-Z]{2}"
    ),
    postcode_o = str_extract(
      `Office / Newsroom Address`,
      "[A-Z]{1,2}[0-9][A-Z0-9]? [0-9][ABD-HJLNP-UW-Z]{2}"
    )
  )

# office postcodes
postcode_office <- observations |>
  select(postcode_o) |>
  drop_na() |>
  distinct() |>
  mutate(postcode_o = str_remove(postcode_o, " "))

pc_list <- postcode_office$postcode_o

# Use purrr::map() to process each chunk
results <- purrr::map_df(pc_list, postcode_lookup)

#####################################################

# # company postcodes
# postcode_company <- observations |>
#   select(postcode_c) |>
#   drop_na() |>
#   distinct() |>
#   mutate(postcode_c = str_remove(postcode_c, " "))
#
# pc_list <- postcode_company$postcode_c
#
# # Use purrr::map() to process each chunk
# results2 <- purrr::map_df(pc_list, postcode_lookup)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# # LET S PUT IT ALL BACK TOGETHER!
# results_all <- bind_rows(results, results2) |>
#   filter(!is.na(quality))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# OWNERSHIP SHARES DATA
ownership <- observations |>
  select(Owner) |>
  group_by(Owner) |>
  mutate(tot_owner = n(),
         perc_owner = round(tot_owner / nrow(observations) * 100, 1)) |>
  distinct() |>
  arrange(desc(tot_owner)) |>
  ungroup() |>
  mutate(cumsum = cumsum(perc_owner))

# do not overwrite now 
# ownership |> write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit#gid=1212780916', sheet = "Ownership shares")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# # OBSERVATIONS UPDATED WITH POSTCODE DATA
observations <-
  observations  |>
  left_join(results, by = c("postcode_o" = "postcode")) |>
  distinct()

# observations |> write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit#gid=1212780916', sheet = "Titles2")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# NEWSROOM LOCATIONS
office_locations <- observations |>
  group_by(latitude) |>
  summarise(across(everything(),
                   ~ paste0(unique(na.omit(
                     .x
                   )), collapse = ", "))) 

# do not overwrite now 
office_locations |> write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit#gid=1212780916', sheet = "Newsrooms locations")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# NEWSROOM BY LAD
office_locations_lad_count <- office_locations |>
  group_by(admin_district) |>
  count()

# do not overwrite now 
office_locations_lad_count |> write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit#gid=1212780916', sheet = "Newsrooms locations LAD count")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# MONOPOLIES
monopolies <-
  observations |>
  separate_rows(`coverage LAD`, sep = "; ") |>
  select(`coverage LAD`, Owner) |>
  group_by(`coverage LAD`) |>
  mutate(tot_admin_district = n()) |>
  group_by(Owner, `coverage LAD`) |>
  mutate(
    tot_owner_admin_district = n(),
    perc_owner_admin_district = round(tot_owner_admin_district / tot_admin_district * 100, 1)
  ) |>
  distinct() |>
  filter(perc_owner_admin_district == 100)

# do not overwrite now unless sure of it
monopolies |> write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit#gid=652214043', sheet = "Monopolies")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# MONOPOLIES BY OWNER
monopolies_count <- monopolies |>
  group_by(Owner) |>
  count()

# do not overwrite now unless sure of it
monopolies_count |> write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit#gid=1212780916', sheet = "Monopolies count")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# MAP OF OWNER TITLES BY LAD
geo_LAD <-
  read_csv("files/Local_Authority_Districts_(December_2022)_Boundaries_UK_BFE.csv")

counts_for_lads <- observations |>
  select(`coverage LAD`, Owner) |>
  group_by(`coverage LAD`) |>
  mutate(tot_admin_district.y = n()) |>
  group_by(Owner, `coverage LAD`) |>
  mutate(
    tot_owner_admin_district.y = 
      n(),
    perc_owner_admin_district.y = 
      round(tot_owner_admin_district.y / tot_admin_district.y * 100, 1)
  ) |>
  distinct() |>
  filter(!is.na(`coverage LAD`)) |>
  ungroup() |>
  pivot_wider(
    id_cols = c(`coverage LAD`, tot_admin_district.y),
    names_from = Owner,
    values_from = perc_owner_admin_district.y
  ) |>
  left_join(observations, by = c("coverage LAD")) |>
  mutate(across(where(is.numeric), function(x)
    tidyr::replace_na(x, 0))) |>
  group_by(`coverage LAD`) |>
  slice_head(n = 1) |>
  right_join(geo_LAD, by = c("coverage LAD" = "LAD22NM"))

# do not overwrite now unless sure of it
counts_for_lads |> write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit#gid=652214043', sheet = "LAD by owner")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# MAP OF TITLES BY LAD
titles_by_lad <-
  observations |>
  group_by(`coverage LAD`) |>
  mutate(tot_admin_district = n()) |>
  ungroup() |>
  group_by(Owner, `coverage LAD`) |>
  mutate(tot_owner_admin_district = n()) |>
  ungroup() |>
  filter(!is.na(`coverage LAD`)) |>
  select(c(
    Owner,
    tot_admin_district,
    tot_owner_admin_district,
    Publication,
    `coverage LAD`
  )) |>
  group_by(`coverage LAD`) |>
  mutate(
    Publication = paste0(unique(na.omit(Publication)), collapse = " - "),
    Owner_long = paste0(unique(na.omit(Owner)), collapse = " - ")
  ) |>
  ungroup() |>
  distinct() |>
  pivot_wider(
    names_from = Owner,
    values_from = tot_owner_admin_district,
    id_cols = c(`coverage LAD`, Publication, Owner_long, tot_admin_district)
  ) |>
  select()

# do not overwrite now unless sure of it
titles_by_lad |> write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1TgCguyAr7EcJgl1vPZvKCFFWZ1zs3spfS6ZZ02ZrrIE/edit#gid=652214043', sheet = "Titles/ Owners in LAD")
