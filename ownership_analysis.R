observations <- read_csv("directory1.csv")

ownership <- observations %>%
  group_by(Publication) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(Owner) %>% 
  group_by(Owner) %>% 
  mutate(tot_owner = n(),
         perc_owner = round(tot_owner / nrow(observations) * 100, 2)) %>% 
  distinct()

ownership_lad <- observations %>% 
  select(LAD, Owner) %>% 
  group_by(LAD) %>% 
  mutate(tot_LAD = n()) %>% 
  group_by(Owner, LAD) %>% 
  mutate(tot_owner_lad = n(),
         perc_owner_lad = round(tot_owner_lad / tot_LAD * 100, 0)) %>% 
  distinct()

histogram <- ownership_lad %>% 
  left_join(counts_for_lads, by = c("LAD")) %>% 
  select(LAD, Owner.x, perc_owner_lad, tot_owner_lad, `Total number of titles in the same Local Authority District`) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c(LAD, tot_owner_lad, `Total number of titles in the same Local Authority District`), names_from = Owner.x, values_from = perc_owner_lad,)

write_csv(histogram, "ownership_shares.csv")
