library(tidyverse)
library(sf)
library(tigris)
library(ggthemes)

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

#counts by race DO NOT NEED THIS
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

#convert victim race to factor

denver_victim_races <- denver_homicides %>%
  mutate(victim_race = as.factor(victim_race)) %>%
  mutate(victim_race = fct_lump(victim_race, n = 3))

ggplot() +
  geom_sf(data = denver_boundaries) +
  geom_sf(data = denver_victim_races, aes(color = victim_race)) 
  
#solved vs unsolved columns
case_status <- denver_victim_races %>%
  mutate(solved = ifelse(disposition %in% c("Closed without arrest", "Closed by arrest"), "Solved", "Unsolved"))


#final plot
ggplot() +
  geom_sf(data = denver_boundaries) +
  geom_sf(data = case_status, aes(color = victim_race), alpha = 0.5) +
  facet_wrap("solved") +
  theme_stata() + 
  labs(
    title = "Denver Homicides by Race", 
    color = "Victim Race"
  )


















