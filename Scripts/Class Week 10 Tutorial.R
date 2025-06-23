
# Preamble ----------------------------------------------------------------


library(HMDHFDplus)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

cat("In today's class we will go through the basic elements of stable population theory. We will examine Net reproductive rate (R0), intrinsic natural rate (r) go back and discuss doubling time. Moreover, we will deepen our knowledge of how to be more efficient in how to code, using functions to avoid repitions in our code.")

# Countries to study ------------------------------------------------------
cat("In this script we will examine three countries over time. Here I chose arbitrarily Austria, Sweden and Korea to have varied time frames and contex")


# We will use this vector to take data from each country
countries <- c("AUS", "SWE","KOR")



# Data processing ---------------------------------------------------------
cat("Here we will introduce a simple function. Function works as such:
in our first part we define the vectors that will be included function (x) and then we set how the function will manipulate the vectors. In this example we are just downloading various country data and then adding some manipulation. But first we need to examine if our female birth ratio assumption holds true. ")

# We need to know whether we can use a general assumption on the female birth ratio, let's examine it:
check <- readHMDweb(CNTRY = "AUS", item = "Births", username =
                      us, password = pw) %>% 
  mutate(proportion = Female/Tot.birth) 


# This is not a nice way to check (I would never do this in a script)
check$proportion %>% 
  mutate(mean_prop = mean(Total))  

cat("The general sex ratio seems to be very close to the constant. You can if you'd want to be more exact to construct it time varying in the following function")

gather_countries <- function(cntry) { # We define cntry as the vector that we will manipulate
  female_sex_ratio               <- 0.4886
  fert <- readHFDweb(cntry, # Here we call the vector
                     username = us, password = pw,
                     item = "asfrRR") # ASFRs
  mort <- readHMDweb(cntry,
                     username = us, password = pw,
                     item = "fltper_1x1") # Female life table
  
  # Merging into one set
  df <- inner_join(fert, mort, by = c("Age", "Year")) %>%
    rename(age = Age, # making everything to lower cases 
           year = Year, 
           lx_raw = lx)    
  
  # Modify ASFR to per woman and L0 correctly
  df <- df %>%
    group_by(year) %>%
    mutate(
      l_x = lx_raw / first(lx_raw),  # ensures l0 = 1 within each year
      m_x = ASFR             # renaming it for the formula
    ) %>%
    ungroup() %>%
    mutate(cntry = cntry)  # creating a variable so we know whose lifetable we are manipulating
  
  
  # Summarise to get R0, T, r_m, DT, λ for each year
  summary <- df %>%
    group_by(cntry, year) %>%
    summarise(
      R0     = sum(l_x * m_x * female_sex_ratio), # Net reproductive rate
      T      = sum(age * (l_x * m_x * female_sex_ratio)) / R0, # average age of childbearing
      r_m    = log(R0) / T, # intristic natural rate
      DT     = log(2)  / r_m, # doubling time
      lambda = exp(r_m), # Annual population growth rate 
      TFR = sum(m_x),
      .groups = "drop"
    )
  
  return(summary) # returns the table from the function
}


# Just applying the function over each string
results <- map_dfr(countries, gather_countries)

# Here we can see that we did it correctly
print(results)


# Plots over the different concepts ---------------------------------------

# 8. R0 over time
p_r0 <- ggplot(results, aes(year, R0, color = cntry)) +
  geom_line(size = 1) +
  labs(title = "R0 over time (1920-2023) by Country (Austria, South Korea and Sweden)",
       x = "year", y = "R0") +
  theme_minimal()

print(p_r0)

# 9. Plot the average age of motherhood
p_t <- ggplot(results, aes(year, T, color = cntry)) +
  geom_line(size = 1) +
  labs(title = "Mean age of childbearing (MAC) over time (1920-2023) by Country (Austria, South Korea and Sweden)",
       x = "Age", y = "mₓ") +
  theme_minimal()

print(p_t)

# 10. Plot reproductive value curves
p_rm <- ggplot(results, aes(year, r_m, color = cntry)) +
  geom_line(size = 1) +
  labs(title = " intrinsic rate of natural increase (r) over time (1920-2023) by Country (Austria, South Korea and Sweden)",
       x = "Age", y = "r") +
  theme_minimal()

print(p_rm)

# 10. Doubling period
p_dt <- ggplot(results %>% filter (!cntry == "KOR"), aes(year, DT, color = cntry)) +
  geom_line(size = 1) +
  labs(title = "Doubling time per throughout each year (1920-2023) by Country(Austria and Sweden)",
       x = "year", y = "R0") +
  theme_minimal()

print(p_dt)

