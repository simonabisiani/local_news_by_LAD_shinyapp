observations <- read_csv("directory1.csv")

ownership <- observations %>%
  group_by(Publication) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(Owner) %>% 
  group_by(Owner) %>% 
  mutate(tot_owner = n(),
         perc_owner = round(tot_owner / nrow(observations) * 100, 1)) %>% 
  distinct() %>% 
  arrange(tot_owner) %>% 
  ungroup() %>% 
  mutate(cumsum = cumsum(perc_owner))

write_csv(ownership, "ownership.csv")

ownership_lad <- observations %>% 
  select(LAD, Owner) %>% 
  group_by(LAD) %>% 
  mutate(tot_LAD = n()) %>% 
  group_by(Owner, LAD) %>% 
  mutate(tot_owner_lad = n(),
         perc_owner_lad = round(tot_owner_lad / tot_LAD * 100, 0)) %>% 
  distinct()


monopolies <- ownership_lad %>% 
  filter(perc_owner_lad == 100) 

write_csv(monopolies, "monopolies.csv")


histogram <- ownership_lad %>% 
  left_join(counts_for_lads, by = c("LAD")) %>% 
  select(LAD, Owner.x, perc_owner_lad, tot_owner_lad, `Total number of titles in the same Local Authority District`) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c(LAD, tot_owner_lad, `Total number of titles in the same Local Authority District`), 
              names_from = Owner.x, values_from = perc_owner_lad,) %>%
  mutate(across(where(is.numeric), function(x) tidyr::replace_na(x, 0))) 
  
write_csv(histogram, "ownership_shares.csv")
