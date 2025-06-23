# ===========================================================================
# The constructed dataset contains information on deaths and population 
# figures for the city of Vienna in the year 2023, as reported by Eurostat.
#
# Deaths     -> Eurostat Table Name: demo_r_magec3 
# Population -> Eurostat Table Name: demo_r_pjangrp3
# ===========================================================================

# Load necessary libraries
library(tidyverse)

# install.packages("eurostat")
library(eurostat)
# -----------------------------------------------------------------------------
#                               DEATHS DATASET
# -----------------------------------------------------------------------------
# Create a data frame for deaths by age group in 2023 (alternative 1)

death_age_groups <- c(
  "Y_LT5", "Y5-9", "Y10-14", "Y15-19", "Y20-24", 
  "Y25-29", "Y30-34", "Y35-39", "Y40-44", "Y45-49",
  "Y50-54", "Y55-59", "Y60-64", "Y65-69", "Y70-74",
  "Y75-79", "Y80-84", "Y85-89", "Y_GE90"
)

deaths_2023 <- c(
  98, 7, 29, 8, 41, 45, 
  64, 73, 128, 120, 94, 661, 
  90, 1229, 1431, 634, 345, 237, 
  3467
)

death_data <- data.frame(
  age_code = death_age_groups,
  deaths_2023 = deaths_2023
)

# Display the deaths data
print(death_data)

# Alternative 2: Use the eurostat package:
deaths_raw <- get_eurostat("demo_r_magec3", time_format = "num") # As we know the data came from demo_r_magec3

vienna_deaths <- deaths_raw %>%  
  filter(geo == "AT13", # Here we filter on Vienna (AT130)
         TIME_PERIOD == 2023, # We want the year 2023
         sex == "T", # And the full population
         !age %in% c("TOTAL", "UNK")) %>% 
  rename(deaths_2023 = values,
         age_code = age) %>%
  select(deaths_2023,age_code)



vienna_deaths_2023 <- vienna_deaths %>%
  mutate(age_code = gsub("^Y[_]?", "", age_code),  # removes both "Y" and "Y_"
         age_code = ifelse(age_code == "LT5", "0-4", age_code), # we specify the outliers
         age_code = ifelse(age_code == "GE90", "90+", age_code), # We do it again for our open-ended bracket
         age = as.numeric(str_extract(age_code, "^\\d+"))) %>% # This takes the first number till the hypen. Don't ask. This is greek to me as well. 
  arrange(age)


# -----------------------------------------------------------------------------
#                             POPULATION DATASET
# -----------------------------------------------------------------------------
# Create a data frame for population figures for January 1st, 2023 and 2024 (Alt 1)

pop_2023 <- c(
  96728, 97981, 93003, 93262, 132668,
  153862, 163864, 149998, 146990, 120304,
  94159, 66546, 90407, 122925, 143134,
  58874, 24084, 17287, 14716
)

pop_2024 <- c(
  95489, 101409, 93849, 96798, 137426,
  165933, 161399, 147909, 144587, 115834,
  90781, 67821, 88326, 130517, 140447,
  57304, 27924, 20881, 14137
)

population_data <- tibble(
  age_code = death_age_groups,
  pop_2023 = pop_2023,
  pop_2024 = pop_2024
)

# Display the population data
print(population_data)

# (Alt 2) Just select the new data and then redo the first alt 2 step
population <- get_eurostat("demo_r_pjangrp3", time_format = "num") # As we know the data came from demo_r_magec3

vienna_population <- population %>%  
  filter(geo == "AT13", # Here we filter on Vienna (AT130)
         TIME_PERIOD %in% c(2023,2024), # We want the year 2023
         sex == "T", # And the full population
         !age %in% c("TOTAL", "UNK")) %>% 
  rename(population = values,
         age_code = age,
         year = TIME_PERIOD) %>%
  select(population,age_code,year)


# Create a proper age variable

vienna_population_2023 <- vienna_population %>%
  filter(year == 2023) %>%
  mutate(
    age_code = gsub("^Y[_]?", "", age_code),  # removes both "Y" and "Y_"
    age_code = ifelse(age_code == "LT5", "0-4", age_code),
    age_code = ifelse(age_code == "GE90", "90+", age_code),
    age = as.numeric(str_extract(age_code, "^\\d+"))
  ) %>%
  arrange(age) %>%
  rename(pop2023 = population) %>%
  select(age, pop2023) %>% 
  filter(!is.na(age)) # There was two open-ended brackets GE 85 and 90

print(vienna_population_2023)

vienna_population_2024 <- vienna_population %>%
  filter(year == 2024) %>%
  mutate(
    age_code = gsub("^Y[_]?", "", age_code),  # removes both "Y" and "Y_"
    age_code = ifelse(age_code == "LT5", "0-4", age_code),
    age_code = ifelse(age_code == "GE90", "90+", age_code),
    age = as.numeric(str_extract(age_code, "^\\d+"))
  ) %>%
  arrange(age) %>%
  rename(pop2024 = population) %>%
  select(age, pop2024) %>% 
  filter(!is.na(age))

print(vienna_population_2024)


# -----------------------------------------------------------------------------
#                            MERGE DEATHS AND POPULATION DATA
# -----------------------------------------------------------------------------
# Merge the deaths and population datasets by age_code

merged_data <- death_data %>%
  left_join(population_data, by = "age_code") %>%
  mutate(age = seq(0, 90, by = 5))

# Select relevant columns
merged_data <- merged_data %>%
  select(age, deaths_2023, pop_2023, pop_2024)

# Alternative 2:
merged_data_alt_2 <- vienna_deaths_2023 %>%
  left_join(vienna_population_2023, by="age") %>%
  left_join(vienna_population_2024, by="age")


# -----------------------------------------------------------------------------
#                          LIFE TABLE CALCULATIONS
# -----------------------------------------------------------------------------
# Define constants
radix <- 100000  # Life table radix
n <- 5           # Age interval length

# Calculate life table columns

# Number of deaths
life_table <- merged_data %>%
  mutate(
    nDx = )

#  average population at risk

life_table <- merged_data %>%
  mutate(
    nmx = ) %>%
  
  # Mortality rate (alias)
  
  mutate(
    nMx = ) %>%
  
  # Average years lived in interval
  
  mutate(
    nax = ) %>%
  
  # Probability of dying
  
  mutate(
    nqx = ) %>% 
  
  # Probability of surviving
  
  mutate(
    npx = ) %>% 
  
  # Survivors at age x
  
  mutate(
    lx = ) %>% 
  
  # Number of deaths in age interval
  
  mutate(
    nLx = ) %>% 
  
  # Person-years lived   
  
  mutate(
    Tx  = ) %>% 
  
  # Total years lived from age x onwards
  
  mutate(
    npx = ) %>% 
  
  # Life expectancy at age x
  
  mutate(
    ex  = ) 


# Congratulations of doing it! Here are some questions:
# 1. which of the two dataframes are correct, why? How could you see that one 
#    might have an error in it?
# 2. Do a ggplot the survival curve.
# 3. Do a ggplot for the mortality rate.
# 4. Create a life table for 2014.
# 5. plot e0 on 2023 and 2014 and compare how it has changed. 
