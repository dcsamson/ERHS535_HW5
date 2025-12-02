library(tidyverse)
library(sf)
library(tigris)

#read in the data
homicides <- read.csv("homicide-data.csv")
#choose a city
print(unique(homicides$city))

denver_homicides <- homicides %>%
  filter(city == "Denver") %>% 
  sf::st_as_sf(coords = c("lat", "lon"),
               crs = 4269)
#check converstion
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

#check the boundaries
ggplot() +
  geom_sf(data = denver_boundaries) +
  geom_sf(data = denver_homicides)





