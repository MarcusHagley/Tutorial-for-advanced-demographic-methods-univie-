######################################################################
# Class 3 Focus:
# - Download data using wcde and HMD
# - Transform data from wide to long and vice versa
# - Joining data.frames together
# - Exploring WIC (wcde) and HMD data
######################################################################

# Project Setup
# 1) Create a new R project (File > New Project). Name it "demography_data"

rm(list = ls())  

# Load Required Packages
library(tidyverse)  # dplyr, ggplot2, readr, tidyr

# Optional: Set Working Directory (uncomment if needed)
# setwd("/path/to/your/directory")

# Install & Load wcde ----------------------------------------------------
# Wittgenstein Centre Data Explorer package
# install.packages("wcde")
library(wcde)

# Explore wcde package:
# ?wcde
help(package = "wcde")

# Some packages have vignettes that you can read about them, wcde has one:
vignette("wcde") # Call the function vignette to access it. 
# Download Total Fertility Rate (TFR) for Austria and Germany (SSP3)
tfr <- get_wcde(
  indicator = "tfr",
  country_name = c("Austria", "Germany"),
  scenario = 3
)

# Find maximum and minimum TFR values:
tfr %>% filter(tfr == max(tfr) | tfr == min(tfr))

# Download population data by education level (Scenario SSP2)
pop_educ <- get_wcde(
  indicator = "pop",
  scenario = 2,
  pop_age = "total",
  pop_sex = "total",
  pop_edu = "four"
)

# Calculate total population across education groups:
pop_total <- pop_educ %>%
  group_by(name, year) %>%
  mutate(pop_total = sum(pop))

# Alternative summarizing method with reframe:
pop_total2 <- pop_educ %>%
  group_by(name, year) %>%
  reframe(pop_total = sum(pop)) # We previously used summarize 

# Load & Compare with UN WPP data --------------------------------------
library(wpp2024)
data(popproj1dt)

un_pop <- popproj1dt %>% 
  rename(country = name, un_pop = pop) %>%
  select(country, year, un_pop)

# Harmonize and join WIC and UN datasets:
pop_comparison <- pop_total2 %>%
  rename(wic_pop = pop_total, 
         country = name) %>%
  inner_join(un_pop, 
             by = c("country", "year")) %>%
  mutate(diff = wic_pop - un_pop)

head(pop_comparison)

# Practice Tasks (WIC data):
# 1) Identify the country/year with greatest discrepancy ("diff").
# 2) Find country with highest population projected for 2065.
# 3) Evaluate WIC projections against UN data since 2022.

# Install & Load Human Mortality Database (HMD) -------------------------
# install.packages("HMDHFDplus")
library(HMDHFDplus)

# Set HMD login credentials (replace with your own):
username <- "your_HMD_username"
password <- "your_HMD_password"

# Download Life Expectancy at birth (E0) for Austria:
e0_AUT <- readHMDweb(
  CNTRY = "AUT",
  item = "E0per_1x1",
  username = username,
  password = password
)

head(e0_AUT)

# Quick Visualization (Austria E0 over time):
ggplot(e0_AUT, aes(x = Year, y = Total)) +
  geom_line(color = "blue") +
  labs(title = "Life Expectancy at Birth (Austria)", x = "Year", y = "E0") +
  theme_minimal()

# Practice Tasks (HMD data):
# 1) Download E0 data for Germany (CNTRY="DEUTNP").
# 2) Combine Austria and Germany data into a single data frame.
# 3) Create a line plot comparing Austria and Germany E0.
# 4) Identify the year with largest E0 difference between countries.
# 5) Calculate and summarize average E0 per country.
# 6) Export the combined dataset to CSV ("hmd_e0_comparison.csv").