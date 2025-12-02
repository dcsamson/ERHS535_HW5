library(tidyverse)
library(sf)
library(tigris)

#read in the data
homicides <- read.csv("homicide-data.csv")

#choose a city
print(unique(homicides$city))

denver_homicides <- homicides %>%
  filter(city == 'Denver') %>% 
  st_as_sf(coords = c("lon", "lat"),
               crs = 4269)

denver_homicides %>%
  ggplot() +
  geom_sf()

#extract denver boundaries from tigris
places <- tigris::places(state = "CO",
                                    cb = FALSE,
                                    year = "2024")
denver_boundaries <- places %>%
  filter(NAME == "Denver")

denver_boundaries %>%
  ggplot() +
  geom_sf()

#align the geometries
ggplot() +
  geom_sf(data = denver_boundaries) +
  geom_sf(data = denver_homicides)

#counts by race
victim_race <- denver_homicides %>%
  group_by(victim_race) %>%
  summarise(
    homicides_by_race = n()
  ) %>%
  arrange(desc(homicides_by_race)) %>%
  slice(1:3) %>%
  print()

#filter to most murdered races

denver_homicides_by_race <- denver_homicides %>%
  filter(victim_race %in% c("Black", "White", "Hispanic"))

