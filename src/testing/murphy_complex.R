
murphy_complex <- mtbs_fire %>%
  st_join(., usa) %>%
  filter(stusps == 'NV') %>%
  filter(fire_name == 'MURPHY COMPLEX') 

# Vector of MODIS tiles to download
tiles <- get_tiles(murphy_complex)
