######################################################################
# Class 7 Focus:
# - Deeper insight into Lee Carter
# - Brief discussion and getting some initial grasp of how Lee-carter works
# - How we can examine how shocks effect our data / model
######################################################################
# Load required packages (NEW! StMoMo)
library(HMDHFDplus)
library(StMoMo) # Install the package if you have to
library(tidyverse)


# Get mortality data from HMD ---------------------------------------------

# Insert your HMD username and password here
username <- "marcus.immonen.hagley@gmail.com"
password <- "Google2004!"

deaths <- readHMDweb("SWE", 
                     "Deaths_1x1", 
                     username, 
                     password)

exposures <- readHMDweb("SWE", 
                        "Exposures_1x1",
                        username,
                        password)



# Get exposures and deaths that we will use to forecast -------------------
# As Miguel discussed in class, Lee-carter is used to forecast and smoothing 
# the different curves. Here we will use Sweden as an example. 


# Manual Lee-Carter -------------------------------------------------------


death_rates <- exposures <- readHMDweb("SWE",
                                       "Mx_1x1",
                                       username,
                                       password)

# In Miguel's Rcode you learned how to do it manually:


df_matrix <- death_rates %>% 
  select(Year,Age,Total) %>% 
  filter(Age %in% seq(0,90)) %>%
  mutate(Total=as.numeric(Total)) %>%
  pivot_wider(
    names_from = Year,
    values_from = Total
  )

mat <- as.matrix(df_matrix[,-1])  # Remove year column
rownames(mat) <- df_matrix$Age   # Assign years as row names


# Step 0: Calculate the log of the mortality rate 
logmx <- log(mat)

# Step 1: Underlying age profile
ax    <- rowMeans(logmx)
plot(ax)


# Step 2: Substract the underlying age profile
M     <- logmx - ax

# Step 3: Decompose the matrix applying the 
# singular value decomposition method
svd_M <- svd(M)

# Step 4: Vector of mortality changes
bx    <- svd_M$u[,1]
plot(bx,xlab="Age")

# Step 5: Vector of time changes
kt    <- svd_M$d[1]*svd_M$v[,1]
# Plot of the time component
plot(colnames(mat),kt,xlab="Year")

# Cross-check: Replication
year <- 2000
plot(logmx[,year-1750],xlab="Age")
lines(ax+bx*kt[year-1750],col="red")





# Using the StMoMo package ------------------------------------------------
cat("Here we will use the StMoMo package. I've added their PDF to moodle so that 
you can read generally about the Lee-carter and all of it's versions. We could've
 use other packages to do this: Demography,ilc, LifeMetrics that would do the same thing.")

# Select relevant columns and filter by age/year
swe_deaths <- deaths %>%
  __(Year, Age, Dx = Total) %>%
  __(as.numeric(Age) <= 90, Year >= 1950)

swe_exposures <- exposures %>%
  __(Year, Age, Ex = Total) %>%
  __(as.numeric(Age) <= 90, Year >= 1950)

# Merge datasets
swe <- inner_join(___, ___, by = c("Year", "Age")) %>%
  mutate(Age = as.numeric(Age), Year = as.numeric(Year)) %>%
  filter(Ex > 0)

# Let's set the ages and years that we are using:
ages <- 0:90
years <- 1950:2023
grid <- expand.grid(Age = ages, Year = years)

# Merge to ensure full coverage and fill in missing values
df_full <- left_join(grid, swe, by = c("Age", "Year")) %>%
  mutate(
    Dx = ifelse(is.na(Dx), 0, Dx),       # Replace missing deaths with 0
    Ex = ifelse(is.na(Ex), NA, Ex)       # Keep NA exposures to avoid div by 0
  )

# Reshape deaths (Dxt) and exposures (Ext) into matrices
Dxt <- df_full %>%
  select(Year, Age, Dx) %>%
  pivot_wider(names_from = Year, values_from = Dx) %>%
  arrange(Age) %>%
  select(-Age) %>%
  as.matrix()

Ext <- df_full %>%
  select(Year, Age, Ex) %>%
  pivot_wider(names_from = Year, values_from = Ex) %>%
  arrange(Age) %>%
  select(-Age) %>%
  as.matrix()

# Redefine age and year vectors to match matrices
ages <- 0:90
years <- 1950:2023

LCmodel <- lc(link = "log")
fitLC <- fit(LCmodel, Dxt = Dxt, Ext = Ext, ages = ages, years = years)

# Plot diagnostics
# First plot: Is the baseline (u-curve), which shows high infant mortality and then the increase over age.
# Second plot: Changes to the baseline (age sensitivity) over time. 
# Third plot: Time trend, so what happens to the overall effect over time. 
plot(fitLC)


#  Now after fitting the model, let's forecast 50 years into the future:
LCfor <- forecast(fitLC, h = 50, simulate = T)
plot(LCfor)


# Extract forecasted mortality rates
rates_forecast <- LCfor$rates  # Matrix: age x forecasted years

# 2. Check which year column corresponds to 2060
colnames(rates_forecast)

# 3. Plot forecasted mortality curve for 2060
forecast_year <- "2060" # Set the forecast year.
ages <- LCfor$ages # Take the ages


# 4
plot(ages, log(rates_forecast[, forecast_year]), type = "l",
     xlab = "Age", ylab = "log(mx)",
     main = "Forecasted log(mx) in 2060",
     col = "blue", lwd = 2)


# We can also add multiple years and compare it to historical lines
years_to_plot <- c("2025", "2045", "2065") # Set years

matplot(ages, log(rates_forecast[, years_to_plot]),
        type = "l", lwd = 2, lty = 1,
        col = c("red", "green", "blue"),
        xlab = "Age", ylab = "log(mx)",
        main = "Forecasted and historical mortality curves (1950,2025,2045,2065)")

#Historical curve
log_mx_1980 <- log(Dxt[, "1950"] / Ext[, "1950"]) # Taking the column 1950 from Dxt and Ext
lines(ages, log_mx_1980, col = "darkgray", lwd = 2, lty = 2)


legend("bottomright",
       legend = c("2030", "2045", "2060", "2073", "Observed 1980"),
       col = c("red", "green", "blue", "black", "darkgray"),
       lty = c(1, 1, 1, 1, 2),
       lwd = 2)

cat("Now we have made, although simplified, our first forecast! Congratulations!
So let us get a deeper understanding of the Lee-carter forecast and why it is more complicated to do in reality.")


# How is Lee-Carter affected by shocks? (e.g earthquakes, covid) --------

# If it happens early:
cat("Here we create a fake shock at the end of the time series
stating that the mortality was 3 times larger in all ages")

# I recommend to always create a new variable, as we might use the old one (which we will)
Dxt_shock_late <- Dxt

# Here we are adding the "shock" in 2020 and 2021:
Dxt_shock_late[, "2020"] <- Dxt_shock_late[, "2020"] * 3
Dxt_shock_late[, "2021"] <- Dxt_shock_late[, "2021"] * 3


# Now we need to redo the Lee-carter fitting and forecasting
fitLC_shock_late <- fit(LCmodel, Dxt = Dxt_shock_late, Ext = Ext, ages = ages, years = years)
fitLC_shock_late <- forecast(fitLC_shock_late, h = 50)
plot(fitLC_shock_late)


# And let's compare our time trend k to understand how this is affected:
plot(years, fitLC$kt, type = "l", lwd = 2, col = "blue",
     ylim = range(c(fitLC$kt, fitLC_shock_late$kt)),
     xlab = "Year", ylab = expression(k[t]),
     main = "Effect of 2020–21 Mortality Shock on k[t]")

lines(years, fitLC_shock_late$kt, col = "red", lwd = 2, lty = 2)

legend("bottomleft",
       legend = c("Original", "With 2020–21 Shock"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Let's compare the estimates at 2060:
plot(ages, log(LCfor$rates[, "2060"]), type = "l", lwd = 2, col = "blue",
     xlab = "Age", ylab = "log(mx)", main = "Forecasted log(mx) in 2060")
lines(ages, log(fitLC_shock_late$rates[, "2060"]), col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = c("Original", "With Shock"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)


# And what happens if the Shock was early?
Dxt_shock_early <- Dxt

# Early shock in the time trend -------------------------------------------
Dxt_shock_early[, "1955"] <- Dxt_shock_early[, "1955"] * 3
Dxt_shock_early[, "1956"] <- Dxt_shock_early[, "1956"] * 3

fitLC_shock_early <- fit(LCmodel, Dxt = Dxt_shock_early, Ext = Ext, ages = ages, years = years)
fitLC_shock_early <- forecast(fitLC_shock_early, h = 50)


# Let's compare the estimates at 2060:
plot(ages, log(LCfor$rates[, "2060"]), type = "l", lwd = 2, col = "blue",
     xlab = "Age", ylab = "log(mx)", main = "Forecasted log(mx) in 2060")
lines(ages, log(fitLC_shock_early$rates[, "2060"]), col = "red", lwd = 2, lty = 2)
lines(ages, log(fitLC_shock_late$rates[, "2060"]), col = "green", lwd = 2, lty = 2)
legend("bottomright", legend = c("Original", "With Shock"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)


# Finally let's look how Lee-carter is affected by shocks in the middle of the time series
middle_year <- as.character(median(1950:2020))
middle_year_1 <- as.character(median(1950:2020)+1)


Dxt_shock_mid <- Dxt

Dxt_shock_mid[, middle_year] <- Dxt_shock_late[,middle_year] * 3
Dxt_shock_mid[, middle_year_1] <- Dxt_shock_late[, middle_year_1] * 3

fitLC_shock_mid <- fit(LCmodel, Dxt = Dxt_shock_mid, Ext = Ext, ages = ages, years = years)
fitLC_shock_mid <- forecast(fitLC_shock_mid, h = 50)


# Full comparison at 2060:
plot(ages, log(LCfor$rates[, "2060"]), type = "l", lwd = 2, col = "blue",
     xlab = "Age", ylab = "log(mx)", main = "Forecasted log(mx) in 2060")
lines(ages, log(fitLC_shock_early$rates[, "2060"]), col = "red", lwd = 2, lty = 2)
lines(ages, log(fitLC_shock_mid$rates[, "2060"]), col = "purple", lwd = 2, lty = 2)
lines(ages, log(fitLC_shock_late$rates[, "2060"]), col = "green", lwd = 2, lty = 2)


legend("bottomright", 
       legend = c("Original", "Shock in 1955–56", "Shock in 1985-1986", "Shock in 2020–21"), 
       col = c("blue", "red", "purple", "green"), 
       lty = c(1, 2, 2, 2), 
       lwd = 2)

# Why are the effects only at younger ages? 
# HINT: what did the diagnostic plot show over  the different dimensions?
# plot(LCfor)

# Do the same analysis for a different country, for example Russia,
# Does it have the same sensitivty age pattern as Sweden?



cat("
Key Takeaways:
- The Lee-Carter model decomposes log mortality into:
  - An age pattern (ax),
  - An age-specific sensitivity to changes (bx),
  - A time trend (kt).
- The model is sensitive to shocks depending on their location in time:
  - Late shocks (like COVID-19) close to the time horizon are seen as noise on future forecasts.
  - Early shocks shift the baseline.
  - Mid-period shocks affect the slope of the trend line (kt) less extend in this particular case than early shocks.
- Lee-Carter both powerful and vulnerable:
  - It captures (historical) trends well,
  - But assumes smooth continuation unless disrupted.
- Lee-Carter and shifts in mortality trends:
  - Lee-Carter is bad att forecasting changes in higher ages (bx)
  - Future changes in life expectancy will be in promoting increased longevity

You should now be more confident in:
- Understanding the inner mechanics of Lee-Carter,
- Diagnosing model behavior via plots,
- Interpreting mortality trend shifts and shock sensitivity.
")



# Optional: Other uses of the Lee-Carter ----------------------------------
cat("Let's continue with Sweden as an example to use the Lee-Carter. As the fundamental
job that the model does is to decompose it to the three dimensions (General pattern, sensitivty and time trend)
we can also use this to impute missing values.

Let's say that the regional hospital due to a fire, lost all their data for a given year.
We can use the lee-carter to fill in the blanks, similar to how we forecasted.
We will forecast and backcast the trend before and after the lost data to recover it. 
This technique is good when you have multiple missing years. As just taking the average
of the closest years is a better fit. The argument is that closer in time more accurately
reflects that time point than the broad strokes that Lee-Carter does.")


# Simulating a lost year in 1980
Dxt_missing <- Dxt
Ext_missing <- Ext

Dxt_missing[, "1980"] <- NA
Ext_missing[, "1980"] <- NA

# 1. Define pre and post missing-year datasets
years_before <- 1950:1979
years_after <- 1981:2023

Dxt_before <- Dxt[, as.character(years_before)]
Ext_before <- Ext[, as.character(years_before)]

Dxt_after <- Dxt[, as.character(years_after)]
Ext_after <- Ext[, as.character(years_after)]

# Fit two separate Lee-Carter models
LC_before <- fit(LCmodel, Dxt = Dxt_before, Ext = Ext_before, ages = ages, years = years_before)
LC_after <- fit(LCmodel, Dxt = Dxt_after, Ext = Ext_after, ages = ages, years = years_after)

# Forecast one step forward from 1979
LC_before_forecast <- forecast(LC_before, h = 1)

# Backward forecast one step back from 1981
# How? Reverse years and interpret backward!
years_reversed <- rev(years_after)  # 2023, 2022, ..., 1981
Dxt_reversed <- Dxt_after[, as.character(years_reversed)]
Ext_reversed <- Ext_after[, as.character(years_reversed)]

LC_after_reversed <- fit(LCmodel, Dxt = Dxt_reversed, Ext = Ext_reversed, ages = ages, years = years_reversed)
LC_after_backcast <- forecast(LC_after_reversed, h = 1)

# Forecasted rates
logmx_1980_forward <- log(LC_before_forecast$rates)
logmx_1980_backward <- log(LC_after_backcast$rates)

# Take average of forward and backward
logmx_1980_estimated <- (logmx_1980_forward + logmx_1980_backward) / 2

# Compare observed vs estimated
observed_1980 <- log(Dxt[, "1980"] / Ext[, "1980"])
observed_1981 <- log(Dxt[, "1981"] / Ext[, "1981"])
observed_1979 <- log(Dxt[, "1979"] / Ext[, "1979"])
avg_observed <- (log(Dxt[, "1981"] / Ext[, "1981"]) + log(Dxt[, "1979"] / Ext[, "1979"]))/2


plot(ages, observed_1980, type = "l", col = "blue", lwd = 2,
     ylab = "log(mx)", xlab = "Age", main = "Observed vs Imputed Mortality in 1980 (Best Practice)")
lines(ages, logmx_1980_estimated, col = "red", lwd = 2, lty = 2)
lines(ages, observed_1979, col = "purple", lwd = 2, lty = 2)
lines(ages, observed_1981, col = "orange", lwd = 2, lty = 2)
lines(ages, avg_observed, col = "black", lwd = 2, lty = 2)

legend("bottomleft",
       legend = c("Observed", "Imputed (forward-backward)",
                  "Observed 1979","Observed 1981","Observed average 1979 & 1981"),
       col = c("blue", "red", "purple","orange","black"), lty = c(1, 2), lwd = 2)

