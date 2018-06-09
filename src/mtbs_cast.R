mtbs_cast <- st_cast(mtbs, to = "POLYGON")

mtbs_cast$duped <- duplicated(mtbs_cast$Fire_ID)

x = table(mtbs_cast$Fire_ID) %>% as_tibble()