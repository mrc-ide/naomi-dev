library(tidyverse)
library(sf)
library(here)

#' Location hierarchy
reg <- here("data-raw", "shapefile", "mw_location_hierarchy.csv") %>% read_csv

#' Construct 32 district shape file from GADM admin2 (https://gadm.org)
tmp <- tempfile()
download.file("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_MWI_2_sf.rds", tmp)
sh2 <- readRDS(tmp)

sh32 <- sh2 %>%
  filter(TYPE_2 != "Water body" | NAME_1 == "Likoma") %>%
  mutate(district = NAME_1,
         district32 = if_else(TYPE_2 == "City", NAME_2, NAME_1)) %>%
  group_by(district, district32) %>%
  summarise() %>%
  ungroup %>%
  left_join(reg) %>%
  select(region, zone, everything()) %>%
  arrange(district32_code) %>%
  mutate(area_id = row_number(),
         region = region %>% fct_inorder(),
         zone = zone %>% fct_inorder(),
         district = district %>% fct_inorder(),
         district32 = district32 %>% fct_inorder())

centers <- sh32 %>% st_centroid %>% st_coordinates %>% as.data.frame
sh32[names(centers)] <- centers


#' Add centroids and adjust to be non-overlapping
sh32 <- sh32 %>%
  mutate(
    Xadj = X %>%
      if_else(district32 == "Lilongwe", X - 0.05, .) %>%
      if_else(district32 == "Mangochi", X - 0.13, .) %>%
      if_else(district32 == "Zomba", X + 0.03, .),
    Yadj = Y %>%
      if_else(district32 == "Lilongwe", Y - 0.17, .) %>%
      if_else(district32 == "Mangochi", Y - 0.12, .) %>%
      if_else(district32 == "Chitipa", Y + 0.18, .) %>%
      if_else(district32 == "Zomba", Y - 0.12, .)
  ) %>%
  rmapshaper::ms_simplify(0.5)

sh32 %>%
  ggplot() +
  geom_sf(aes(fill = zone), alpha = 0.4) +
  geom_point(aes(Xadj, Yadj), color = "red") +
  geom_point(aes(X, Y))

save(sh32, file = here::here("data", "shapefile.rda"))

#' Review districts by region
par(ask = TRUE)

sh32 %>%
  split(.$zone) %>%
  lapply(function(x)
    x %>% 
    ggplot(aes(fill = district32)) +
    geom_sf() +
    ggtitle(x$zone[1])
    )
