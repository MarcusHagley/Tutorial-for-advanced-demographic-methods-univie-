# Class 12: Decomposition 
# 2025S 234001-1 Applied Methods of Demographic Analysis (Tutorial)
# Learning Objectives:
# - Understand the purpose of demographic decomposition methods
# - Apply Kitagawa decomposition to crude death rates (CDR)
# - Distinguish between composition effects and rate effects
# - Practice transforming and joining demographic datasets from HMD
# - Visualize decomposition contributions by age and sex

rm(list=ls())

# Preamble ----------------------------------------------------------------
library(HMDHFDplus)
library(tidyverse)
library(ggplot2)


# Comparison years
years <- c(2005, 2019)

# Country
country    <- "USA"


# Set passwords to Sys.setenv(us = "", pw = "")
us <- Sys.getenv("us")
pw <- Sys.getenv("pw")
# Data processing ----------------------------------------------------------
cat("We’ll decompose the USA’s CDR by age & sex between 2005 and 2019 using Kitagawa. \n",
    "We pull exposures and age- and sex-specific mortality rates (mx) from HMD for 2005 and 2019, \n",
    "then see how age‐sex composition vs. age‐sex rate changes explain ΔCDR.\n")

# download the data first exposures (what are we doing to the data?)
exposure <- readHMDweb(
  CNTRY    = country,
  item     = "Exposures_1x1",
  username = us,
  password = pw
) %>%
  filter(Year %in% years) %>% # Take the years we want 
  select(Year, Age, Female, Male) %>% 
  pivot_longer(
    cols      = c(Female, Male),
    names_to  = "Sex",
    values_to = "Exposure"
  ) %>%
  mutate(Sex = ifelse(Sex == "Male", "M", "F")) %>% 
  select(Year, Sex, Age, Exposure)

# Download mx
mx <- readHMDweb(
  CNTRY    = country,
  item     = "Mx_1x1",
  username = us,
  password = pw
) %>%
  filter(Year %in% years) %>%
  select(Year, Age, Female, Male) %>%
  pivot_longer(
    cols      = c(Female, Male),
    names_to  = "Sex",
    values_to = "mx"
  ) %>%
  mutate(Sex = ifelse(Sex == "Male", "M", "F")) %>%
  select(Year, Sex, Age, mx)


# Join the tables together
ps_raw <- left_join(
  exposure,
  mx,
  by = c("Year", "Sex", "Age")
) %>%
  mutate(mx = ifelse(is.na(mx), 0, mx)) # why?

# For Kitagawa decomposition we need to get the share of exposures by each age within each years.
ps <- ps_raw %>%
  group_by(Year) %>%
  mutate(
    total_pop = sum(Exposure),         # Get the total
    pop_share   = Exposure / total_pop   # share of total population
  ) %>%
  ungroup() %>%
  select(Year, Sex, Age, pop_share, mx)


head(ps)

# Compute the crude
cdr <- ps %>%
  group_by(Year) %>%
  summarize(
    CDR = sum(pop_share * mx),  
    .groups = "drop"
  )


print(cdr) # Good to know our crude death rates to see that everything else is right


ps_wide <- ps %>%
  filter(Year %in% years) %>%
  select(Year, Sex, Age, pop_share, mx) %>%
  pivot_wider(
    names_from  = Year,
    values_from = c(pop_share, mx),
    names_glue  = "{.value}_{Year}"
  ) %>%
  rename(
    p_b = pop_share_2005,  # _b is 2005
    m_b = mx_2005,       
    p_a = pop_share_2019,  # _a is 2019
    m_a = mx_2019        
  ) %>%
  mutate(
    p_b = ifelse(is.na(p_b), 0, p_b),
    m_b = ifelse(is.na(m_b), 0, m_b),
    p_a = ifelse(is.na(p_a), 0, p_a),
    m_a = ifelse(is.na(m_a), 0, m_a)
  )

head(ps_wide)

# Check again
CDR_B2 <- sum(ps_wide$p_b * ps_wide$m_b)  # Should match cdr$CDR for 2010
CDR_A2 <- sum(ps_wide$p_a * ps_wide$m_a)  # Should match cdr$CDR for 2019
total_gap2 <- CDR_A2 - CDR_B2

cat("CDR (2005) = ", round(CDR_B2, 4), "initial CDR (2005) = ",round(cdr$CDR[1],4), "\n")
cat("CDR (2019) = ", round(CDR_A2, 4), "initial CDR (2019) = ",round(cdr$CDR[1],4), "\n")
cat("Δ CDR      = ", round(total_gap2, 6), "\n\n")




# Calculate the compositional and rate effects ----------------------------

# Composition effect = change in population age-sex structure * mortality of base year
# Rate effect        = change in mortality rates * population structure of comparison year

# Composition effect = Σ (pop_share_a - pop_share_b) * mx_b (standardizing mx)
comp_effect2 <- sum((ps_wide$p_a - ps_wide$p_b) * ps_wide$m_b)

# Rate effect = Σ (mx_a - mx_b) * pop_share_a (standardizing composition)
rate_effect2 <- sum((ps_wide$m_a - ps_wide$m_b) * ps_wide$p_a)

# Check difference
raw_diff2 <- (comp_effect2 + rate_effect2) - total_gap2

cat("Composition effect = ", round(comp_effect2, 6), "\n")
cat("Rate effect        = ", round(rate_effect2, 6), "\n")
cat("(Comp + Rate) - ΔCDR = ", round(raw_diff2, 8), "\n")
cat("Abs difference      = ", round(abs(raw_diff2), 8), "\n\n")


# Contribution ------------------------------------------------------------


contributions <- ps_wide %>%
  transmute(
    Sex,
    Age,
    Comp_contribution = (p_a - p_b) * m_b,
    Rate_contribution = (m_a - m_b) * p_a
  )

# Top years contributing to changes in composition
print(
  contributions %>%
    arrange(desc(abs(Comp_contribution))) %>%
    slice(1:10)
)

# Top years contributing to changes in rates
print(
  contributions %>%
    arrange(desc(abs(Rate_contribution))) %>%
    slice(1:10)
)


# Plot it 
cat("Our current dataframe is in wide (check for yourself), now to use ggplot we need long format")

plot_dataframe <- contributions %>%
  pivot_longer(
    cols      = c(Comp_contribution, Rate_contribution),
    names_to  = "effect",
    values_to = "contribution"
  )

ggplot(plot_dataframe, aes(
  x    = Age,
  y    = contribution,
  fill = effect
)) +
  geom_col(position = "dodge") +
  facet_wrap(~Sex, nrow = 2, scales = "free_y") +
  labs(
    title    = paste("Kitagawa decomposition CDR in",country,"(between 2005 and 2019) by age & sex"),
    subtitle = "Blue = rate changes Red = population composition",
    x        = "Single‐year Age",
    y        = "Contribution to Δ CDR"
  ) +
  theme_minimal()

cat(" Negative rates means that the rate have decreased. Positive means that it has increased in 2019 compared to 2005. ")
