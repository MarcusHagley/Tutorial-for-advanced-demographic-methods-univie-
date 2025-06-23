# Class 13: Introduction to Spatial Demography with R
# 2025S 234004 UE Tutorial to Advanced Methods of Demographic Analysis
# In this class we will use concepts and code from *Geocomputation with R*: https://r.geocompx.org/
# Instructor: Marcus Immonen Hagley

# Learning Objectives:
# - Load and handle spatial data using built-in R packages
# - Create and interpret thematic maps
# - Understand coordinate reference systems (CRS)
# - Perform basic spatial analysis relevant to demography


# Preamble ----------------------------------------------------------------

# Install required packages if not already installed:
# install.packages("sf")
# install.packages("terra")
# install.packages("spData")
# install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")
# install.packages("WDI")

library(sf)            # Handles vector geographic data
library(terra)         # Handles raster data
library(spData)        # Sample spatial datasets
library(spDataLarge)   # Additional large spatial datasets
library(leaflet)       # Interactive maps
library(tidyverse)     # General data wrangling
library(WDI)           # Getting life expectancy data

set.seed(123)


# Spatial Data Concepts --------------------------------------------------

cat("We are going to use sf objects. sf objects can represent all common vector geometry types: points, lines, polygons, and their multi-versions. Raster data are handled separately using terra.")


# Interactive Map: Where did we get our PhDs from? ------------------------
popup <- c("Marcus", "Miguel", "Eva", "Erich")

leaflet() |> 
  addProviderTiles("Esri.WorldImagery") |> 
  addMarkers(lng = c(11.2825, -3.69611, 2.3455, 16.4085),
             lat = c(43.80293, 40.5453, 48.8474, 48.2134), 
             popup = popup)


# Exploring Built-in Vector Data -----------------------------------------

# Plot life expectancy by country from the `world` dataset 
data(world)
plot(world["lifeExp"])
summary(world["lifeExp"])

# Examine sf object structure
class(world)
head(world)

# Filter to Europe only
europe <- world %>% 
  filter(continent == "Europe")

plot(europe["pop"])


# Reading sf data from file ----------------------------------------------

world_dfr <- st_read(system.file("shapes/world.gpkg", package = "spData"))
plot(world_dfr[3:6])


# Highlighting and Manipulating Spatial Objects --------------------------

# Highlight Europe
world_eu <- world %>% filter(continent == "Europe")
eu <- st_union(world_eu)

plot(world["pop"], reset = FALSE)
plot(eu, add = TRUE, col = "red")

# Highlight France and surrounding countries
france <- world %>% filter(name_long == "France")

plot(st_geometry(france), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(world_eu), add = TRUE)


# Raster Data: Elevation -------------------------------------------------

raster_filepath <- system.file("raster/srtm.tif", package = "spDataLarge")
my_rast <- rast(raster_filepath)
class(my_rast)
plot(my_rast)


# Working with Units -----------------------------------------------------

luxembourg <- world %>% filter(name_long == "Luxembourg")

# Area in default units
st_area(luxembourg)

# Convert to square kilometers
units::set_units(st_area(luxembourg), km^2)

# Countries with area < 10,000 km²
i_small <- world$area_km2 < 10000
small_countries <- world[i_small, ]


# Spatial Aggregation Example --------------------------------------------

canterbury <- nz %>% filter(Name == "Canterbury")
canterbury_height <- nz_height[canterbury, ]
nz_height[canterbury, , op = st_disjoint]

# Aggregate elevation by region
nz_agg <- aggregate(x = nz_height, by = nz, FUN = mean)

nz_agg2 <- st_join(x = nz, y = nz_height) |> 
  group_by(Name) |> 
  summarize(elevation = mean(elevation, na.rm = TRUE))


# Merging with External Data ---------------------------------------------

# Let's add the coffee data that we already have from the libraries:
world_coffee <- left_join(world, coffee_data)
plot(world_coffee["coffee_production_2017"])


# Let's do more demographical things of interest: Updating our map with life expectancies

# Let's get the current life expectancy
life_exp_latest <- WDI(country = "all", 
                       indicator = "SP.DYN.LE00.IN",
                       start = 2023, end = 2023)

pop_density_2022 <- WDI(country = "all", 
                        indicator = "EN.POP.DNST", start = 2022, end = 2022) %>% 
  rename(country_name = country,
         pop_den_2022 = EN.POP.DNST)

# Let's rename it so it has the same names for easy merg
life_exp_latest <- life_exp_latest %>%
  rename(country_name = country,
         lifeExp_2023 = SP.DYN.LE00.IN)


world_updated <- left_join(world, life_exp_latest,
                           by = c("name_long" = "country_name"))


world_updated <- left_join(world, pop_density_2022,
                           by = c("name_long" = "country_name"))

plot(world_updated[c("pop_den_2022")])

plot(world_updated[c("lifeExp","lifeExp_2023")])

plot(world_updated["lifeExp_2023"])

# What if I want to highlight the countries with very high life expectancy?

# Filter countries with life expectancy > 80
high_life <- world_updated %>% filter(lifeExp_2023 > 80)

# Plot all countries
plot(st_geometry(world_updated), col = "lightgray", main = "Countries with Life Expectancy > 80")

# Add those with high life expectancy in transparent fill but thick border
plot(st_geometry(high_life), add = TRUE, border = "red", lwd = 2)

