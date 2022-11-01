library(sf)
library(rmapshaper)
library(tidyverse)

ecoregions <- read_sf("extdata/ecoregions")

ecoregions <- ms_simplify(ecoregions, 0.05)

ecoregions_realm <- ecoregions %>%
  dplyr::select(ECO_NAME, BIOME_NAME, REALM) %>%
  group_by(REALM) %>%
  group_nest(keep = TRUE) %>%
  filter(REALM != "N/A") %>%
  mutate(data = map(data, st_make_valid))

names(ecoregions_realm$data) <- ecoregions_realm$REALM

attach(ecoregions_realm$data)

usethis::use_data(Afrotropic, Australasia,
                  Indomalayan, Nearctic,
                  Neotropic, Oceania,
                  Palearctic,
                  overwrite = TRUE)
