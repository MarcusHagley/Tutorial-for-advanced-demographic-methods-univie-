# ===========================================================================
# Class 6: Parametric Mortality Models in R
#
# In this class, you will:
#  - Learn how mortality changes with age can be described by simple models
#  - Use the `MortalityLaws` package to fit models like Gompertz, Makeham,
#    and Kannisto to real mortality data from Sweden
#  - Plot and compare model fits visually
#
# At the end, you should understand how different models fit different 
# parts of the age range and how to evaluate model performance.
# ===========================================================================
rm(list=ls())
# Load necessary libraries
library(tidyverse)
library(HMDHFDplus)

# install.packages("MortalityLaws")
library(MortalityLaws)


# Download data -----------------------------------------------------------
# This week we will continue using the mortality data from the human mortality
# database (HMD) to fit various models that try to take into account the law-like
# behaviour of the mortality rate in populations.
#
# I derive the mx again but we can also just take it from the e0 file.


username <- ""
password <- ""

deaths_swe <- readHMDweb(CNTRY = "SWE", 
                         item = "Death1x1", 
                         username = username, 
                         password = password)

population_swe <- readHMDweb(CNTRY = "SWE", 
                             item = "Population", 
                             username = username, 
                             password = password)

# Alt 2: Use the file that I uploaded on Moodle. the HMDHFDplus did not work when
# I was writing the script. Don't forget to setwd ().
library(readxl)
deaths_swe <-  read_excel("Sweden_mortality_2023.xlsx")
population_swe <-  read_excel("Sweden_population_20232024.xlsx")

# data manipulations just to get the mx using alt 2.
population_swe_wide <- pivot_wider(
  population_swe,
  names_from = year,
  values_from = pop_total,
  names_glue = "year_{year}"
) %>% 
  mutate(mid_year_pop = (year_2023 + year_2024)/2)


population_mx <- population_swe_wide %>% 
  left_join(deaths_swe, by ="age") %>%
  mutate(mx = deaths / mid_year_pop,
         age = as.numeric(age),
         age = ifelse(is.na(age),110,age))



# Restricting model -------------------------------------------------------

population_mx_restrict <- population_mx %>%
  filter(age > 34)


# Fitting models ----------------------------------------------------------
# While gompertz is easy to fit using it manually, as it is just a linear estimation
# of the log transformed mortality rate, other para are more difficult. 

#Here we will fit gompertz using two different ways. First using the package:

fit_gompertz <-   MortalityLaw(x = population_mx_restrict$age, 
                               mx = population_mx_restrict$mx, law = "gompertz")

summary(fit_gompertz)

# Second fitting it ourselves using a linear equation and log transforming the 
# mx:
gompertz <- lm(log(mx) ~ age, data = population_mx_restrict)
alpha_gompertz <- exp(coef(gompertz)[1])
beta_gompertz <- coef(gompertz)[2]

# Compare parameters
cat("Manual Gompertz: alpha =", round(alpha_gompertz, 6), 
    "| beta =", round(beta_gompertz, 4), "\n")


# Here you can see that the intercept (alpha) is 0 on both and that the age coef
# both have 0.0882

# For more difficult models we will need to use the package to not make things
# difficult for us. For kannisto:

fit_kannisto <-   MortalityLaw(x = population_mx_restrict$age, 
                               mx = population_mx_restrict$mx, law = "kannisto")

summary(fit_kannisto)

fit_kannisto_makeham <-   MortalityLaw(x = population_mx_restrict$age, 
                                       mx = population_mx_restrict$mx, law = "kannisto_makeham")
summary(fit_kannisto_makeham)

# Makeham
fit_makeham <-   MortalityLaw(x = population_mx_restrict$age, 
                              mx = population_mx_restrict$mx, law = "makeham")
summary(fit_makeham)

# Visualize and evaluate ---------------------------------------------------

# Create fitted values
compare_data <- population_mx_restrict %>%
  mutate(
    mx_fit_gompertz = fit_gompertz$fitted.values,
    fit_kannisto_makeham = fit_kannisto_makeham$fitted.values,
    fit_makeham = fit_makeham$fitted.values
  )

ggplot(compare_data, aes(y = mx,x = age)) +
  geom_point(aes(y = mx), color = "black", size = 1.2) +
  geom_line(aes(y = mx_fit_gompertz), color = "blue", linewidth = 1) +
  geom_line(aes(y = fit_kannisto_makeham), color = "red", linewidth = 1) +
  geom_line(aes(y = fit_makeham), color = "purple", linewidth = 1) +
  scale_y_log10() +
  labs(
    title = "Fitted Mortality Models vs Observed (log scale)",
    subtitle = "Black = observed, Blue = Gompertz, Red = Kannisto0-Makeham, Purple = Makeham",
    y = "Mortality rate (log scale)", x = "Age"
  ) +
  theme_minimal()

# The most common way to evaluate models is to use the BIC or AIC information
# We won't do it in this class. But we can take the information from the models
# to understand which model is the best fit given the extra information we require
# from it.




# Exercise ---------------------------------------------------------------
#  Try fitting Gompertz to ages 25+ instead of 35+ — how does it affect the fit?
#  What happens to the fitted curves if you include younger ages (say 20+)?
#  Which model fits best visually?
#  Do any of the models seem to over- or under-predict mortality at the oldest ages?
