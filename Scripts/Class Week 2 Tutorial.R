######################################################################
# Class 2 Focus:
# - Working directories: getwd(), setwd()
# - File paths and vectors
# - Summaries (summarize/summarise)
# - Additional ggplot layers (e.g., geom_histogram)
# - rename()
# - table() function
# - write.csv() / fwrite() with file paths
# - ifelse()
# - Getting help with '?'
######################################################################

######################################################################
# Project Setup
# 1) Create a new R project (File > New Project). Name it "employment"
######################################################################

# Demonstration & Reminders:

# 0. Working Directories
# getwd() returns the current working directory:
cat("Current working directory is:", getwd(), "\n")

# setwd() can be used to change directories (uncomment if needed):
# setwd("/path/to/your/folder")

# 1. Creating File Paths:
# We can safely build file paths with file.path():
example_csv_path <- file.path("/path", "to", "my_data", "example.csv")
cat("Example CSV path:", example_csv_path, "\n")

# 2. Getting Help:
# Prepend a function name with '?' for documentation, e.g.:
# ?mean
# ?ifelse

# 3. table() usage:
# Demonstration will come after we load some data.

# ---------------------------------------------------------------

# 1. Clean the Environment
rm(list = ls())  # Removes all objects from the current R environment

# 2. Load Required Packages
library(tidyverse)  # Includes dplyr, readr, ggplot2, etc.

# 3. Set File Paths (Optional)
# This is a placeholder for any paths you might use for reading/writing files:
# data_path <- "~/my_data_folder/some_file.csv"
# output_path <- "~/my_output_folder/"

# 4. Download & Import Data
# As an example, you could use download.file() and read_csv() to import from the internet:
# download.file("https://example.com/my_data.csv", destfile = "my_data.csv")
# my_data <- read_csv("my_data.csv")

# Typically, you'd set up a new project (Project > New Project) before these steps.



# Starting to use WPP from University of Washington PPG -------------------


# 5. Installing a Package from GitHub
# wpp2024 is not on CRAN, so we use devtools to install from GitHub.

install.packages("devtools")         # Installs devtools from CRAN
options(timeout = 600)               # Extend download timeout if needed
library(devtools)
install_github("PPgp/wpp2024")       # Installs wpp2024 from GitHub

# Once installed, load the packages we need:
library(wpp2024)
library(dplyr)
library(ggplot2)

# Learn more about wpp2024:
?wpp2024

# The wpp2024 package provides sample population data:
data(pop)    # 5-year intervals in wide format
data(pop1)   # 1-year intervals in wide format
data(pop1dt) # 1-year intervals in long format

# We can remove data.frames we don't need:
rm(pop1, pop)



# Aggregate summaries and rename -----------------------------------------------------
# 6. Summaries
# group_by() + summarize() aggregates data by specified groups.

population_aggregates <- pop1dt %>%
  group_by(name) %>%              # Group by country/region name
  summarize(
    avg_populaton    = mean(pop, na.rm = TRUE),   # Average population
    median_population = median(pop, na.rm = TRUE), # Median population
    sd_population     = sd(pop, na.rm = TRUE),     # Standard deviation of population
    min_year          = min(year),                 # Earliest year in data
    max_year          = max(year),                 # Latest year in data
    count             = n()                        # Number of rows (observations)
  ) %>%
  ungroup()  # Ungroup so subsequent operations don't use "name" as a grouping variable

print(population_aggregates)

# 6a. Selecting Columns
# select() picks specific columns from a data frame.

population_selected <- pop1dt %>%
  select(name, year, pop)

# head(..., 10) shows the first 10 rows:
print(head(population_selected, 10))

# 7. More Summaries, rename(), table()
# rename() changes column names.

population_selected <- population_selected %>%
  rename(country_name = name)

# table() quickly displays frequency counts or cross-tabulations:
print(table(population_selected$country_name, population_selected$pop))

# 7. Exporting Data
# We can export data frames to CSV with write.csv() or data.table's fwrite().

# install.packages("data.table")
# library(data.table)
# fwrite(population_aggregates, file = "population_aggregates.csv")

# Or using write.csv (built-in):
# write.csv(population_aggregates, file = "population_aggregates.csv", row.names = FALSE)

# 8. Basic Visualization
# Example: line plot of population over time by country



# Visualization of regions ------------------------------------------------


ggplot(population_selected, aes(x = year, y = pop, color = country_name)) +
  geom_line() +
  labs(title = "Countries by population", x = "Years", y = "Population")

# This is cluttered if we plot too many countries. Let's filter down to regions.

# Check available country/region names:
population_selected %>%
  distinct(country_name) %>%  # Unique names only
  arrange(country_name) %>%   # Sort alphabetically
  pull(country_name)          # Print the list

# Filter to selected regions:
population_selected_regions <- population_selected %>%
  filter(country_name %in% c(
    "Africa",
    "Europe",
    "Asia",
    "South America",
    "Northern America",
    "Oceania",
    "World"
  ))

# Double-check the filtered dataset:
population_selected_regions %>%
  distinct(country_name) %>%
  arrange(country_name) %>%
  pull(country_name)

# Plot the filtered data:
ggplot(population_selected_regions, aes(x = year, y = pop, color = country_name)) +
  geom_line(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Countries by population",
    x = "Years",
    y = "Population",
    color = "Region"     # Legend title
  ) +
  theme_minimal()



# Growth and doubling time ------------------------------------------------


# Calculate annual growth rate (year-over-year) for all countries in pop1dt:
population_growth <- pop1dt %>%
  arrange(name, year) %>%  # Sort by country and year first
  group_by(name) %>%
  mutate(
    pop_previous = lag(pop),  # population in the previous year
    # Growth rate in percent from t-1 to t
    growth_rate_percent = 100 * (pop - pop_previous) / pop_previous
  ) %>%
  ungroup()

# First row per group will have NA for growth_rate_percent (no previous year).
head(population_growth, 10)

# Summarize average annual growth rate for major regions:
annual_growth_summary <- population_growth %>%
  filter(name %in% c(
    "Africa",
    "Europe",
    "Asia",
    "South America",
    "Northern America",
    "Oceania",
    "World"
  )) %>%
  group_by(name) %>%
  summarize(
    avg_growth_rate = mean(growth_rate_percent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_growth_rate))

annual_growth_summary

# Compare population in 1950 vs. 2023 for major regions:
pop_1950_2023 <- pop1dt %>%
  filter(
    year %in% c(1950, 2023),
    name %in% c("Africa","Europe","Asia","South America","Northern America","Oceania","World")
  ) %>%
  select(name, year, pop) %>%
  arrange(name, year)

pop_1950_2023

# Calculate approximate average log growth rate from 1950 to 2023:
growth_1950_2023 <- pop_1950_2023 %>%
  group_by(name) %>%
  summarize(
    pop_1950 = pop[year == 1950],
    pop_2023 = pop[year == 2023]
  ) %>%
  mutate(
    avg_log_growth_50yr = (1 / (2023 - 1950)) * log(pop_2023 / pop_1950)
  )

growth_1950_2023

# Estimate doubling times:
doubling_times <- growth_1950_2023 %>%
  mutate(doubling_time = log(2) / avg_log_growth_50yr)

doubling_times


# Curiosa -----------------------------------------------------------------
# So how fast can a population double? The fastest growth rate in a human population
# in 2023 was Oman with 6.5 percent and in 1978 UAE had a 13.1 growth rate.
# when would they double?

doubling_time_Oman_2023 <- log(2) / ###
  print(doubling_time_Oman_2023)

doubling_time_UEA_1978 <- log(2) / ####
  print(doubling_time_UEA_1978)

# Continue with doubling times --------------------------------------------


# Identify the minimum and maximum doubling times:
doubling_times %>% filter(doubling_time == min(doubling_time))
doubling_times %>% filter(doubling_time == max(doubling_time))

# Example: categorize growth into 'High Growth (>1%)' vs. 'Low/Moderate'
population_growth <- population_growth %>%
  mutate(
    growth_category = ifelse(
      growth_rate_percent > 1.0, 
      "High Growth (>1%)", 
      "Low/Moderate Growth (<=1%)"
    )
  )


# More advanced task: Indonesia -------------------------------------------
# Taken from Wachter (2014) Essential Demographic Methods:
# Between 1960 and 1975 the population of Indonesia grew from about
# 100.6 million to 137.5 million. Between 1990 and 2000 it grew from
# 187.7 million to 225.0 million. Find the growth rates between 1960 and
# 1975 and between 1990 and 2000. Have the growth rates increased or
# declined? How would we do this?

#Here we want a wideformat so we use pop1
data(pop1)
#First take Indonesia and the specific dates:
pop_indonesia <- pop1 %>%
  filter(name == "Indonesia") %>%
  select(name, "1960","1975","1990","2000")

# Let's see if the UN has a different estimates than 
pop_indonesia

# Calculate the average growth rate:
r_1960_1975 <- (1 / 15) * log(pop_indonesia$`1975` / pop_indonesia$`1960`)
r_1960_1975

# Calculate the total log growth rate:
r_1960_1975_total <- log(pop_indonesia$`1975` / pop_indonesia$`1960`)
exp(r_1960_1975_total)

# Calculate the 10 years
r_1990_2000 <- (1 / 10) * log(pop_indonesia$`2000` / pop_indonesia$`1990`)
r_1990_2000

# Calculate the total log growth rate
r_1990_2000_total <-log(pop_indonesia$`2000` / pop_indonesia$`1990`)
exp(r_1990_2000_total)

# calculate the anual percentage 
annual_rate_percent_1960_1975 <- 100 * (exp(r_1960_1975) - 1)
print(annual_rate_percent_1960_1975) 
cat("Each year, the population increased by",annual_rate_percent_1960_1975, "percent")

annual_rate_percent_1990_2000 <- 100 * (exp(r_1990_2000) - 1)
print(annual_rate_percent_1990_2000)
cat("Each year, the population increased by",annual_rate_percent_1990_2000, "percent")



# Check if one is larger or they are similar:

if (r_1960_1975 > r_1990_2000) {
  cat("Growth rate from 1960 to 1975 is higher than the rate between 1990 to 2000.\n")
} else if (r_1960_1975 < r_1990_2000) {
  cat("Growth rate from 1990 to 2000 is higher than the rate between 1970 to 1975.\n")
} else {
  cat("Both growth rates are equal.\n")
}


# 9. Additional Practice Tasks
# -------------------------------------------------------------------
# Suppose you have an "employees" CSV in your Downloads folder, and you want to read it:

directory_path  <- "C:/Users/x/Downloads"
filename        <- "X.csv"
filepath        <- file.path(directory_path, filename)

employees <- read.csv(filepath)
head(employees)

# 1) Create a 'bonus' column = 10% of the salary
# employees <- employees %>% mutate(bonus = salary * 0.1)

# 2) Group by 'salary_level' and calculate the average 'performance_score'
# salary_perf <- employees %>%
#   group_by(salary_level) %>%
#   summarize(avg_perf = mean(performance_score, na.rm = TRUE))
# print(salary_perf)

# 3) Save your updated 'employees' data as "employees_updated.csv"
# write.csv(employees, file = "employees_updated.csv", row.names = FALSE)

# 4) Create a bar plot of 'salary_level' counts, colored by 'department'
# ggplot(employees, aes(x = salary_level, fill = department)) +
#   geom_bar(position = "dodge") +
#   labs(title = "Count by Salary Level", x = "Salary Level", y = "Count")

# 9. Additional Practice Tasks (Fill in the missing pieces)

# A. Use mutate() to create a new column called 'bonus' which is 10% of the salary.
employees <- employees %>% mutate(bonus = ### * 0.1)
                                    
                                    # B. Group by 'salary_level' and calculate the average performance_score.
                                    salary_perf <- employees %>%
                                    group_by(###) %>%
                                      summarize(avg_perf = mean(###, na.rm = TRUE))
                                        
                                        print(salary_perf)
                                        
                                        # C. Save your updated 'employees' data as a CSV file named "employees_updated.csv".
                                        write.csv(employees, file = ###, row.names = FALSE)
                                                    
                                                    # D. Create a bar plot of 'salary_level' counts, colored by 'department'.
                                                    ggplot(employees, aes(x = ###, fill = ###)) +
                                                                            geom_bar(position = "dodge") +
                                                                            labs(title = "", x = "", y = "")
                                                                          
                                                                          
                                                                          ######################################################################################
                                                                          #                ADDITIONAL PRACTICE TASKS            #
                                                                          #                (Tasks #5 through #44)               #
                                                                          ######################################################################################
                                                                          
                                                                          # 5. Create a new column 'perf_category' using nested ifelse() based on 'performance_score':
                                                                          #    - 'Low' if < 3
                                                                          #    - 'Medium' if 3 or 4
                                                                          #    - 'High' if 5
                                                                          employees <- employees %>% mutate(perf_category = ifelse(### < 3, "Low", ifelse(### < 5, "Medium", "High")))
                                                                            
                                                                            # 6. Filter 'employees' to only include those with a 'performance_score' of 5. Call it 'perfect_scores'.
                                                                            perfect_scores <- employees %>% filter(### == ###)
                                                                              print(perfect_scores)
                                                                              
                                                                              # 7. Arrange 'employees' in descending order of 'salary'.
                                                                              employees_desc <- employees %>% arrange(desc(###))
                                                                                head(employees_desc)
                                                                                
                                                                                # 8. Create a new column 'salary_norm' that normalizes salary (subtract the minimum salary, divide by the range).
                                                                                employees <- employees %>% mutate(salary_norm = (salary - min(salary)) / (max(salary) - min(salary)))
                                                                                
                                                                                # 9. Group by 'department' and calculate the median of 'performance_score'.
                                                                                dept_perf_median <- employees %>% group_by(###) %>% summarize(median_perf = median(###, na.rm = TRUE))
                                                                                  print(dept_perf_median)
                                                                                  
                                                                                  # 10. Rename 'perf_label' to 'performance_label'.
                                                                                  employees <- employees %>% rename(performance_label = ###)
                                                                                                                      
                                                                                                                      # 11. Create a new data frame containing only rows where 'department' is 'Finance'.
                                                                                                                      finance_employees ### employees %>% filter(### == "Finance")
                                                                                                                    print(finance_employees)
                                                                                                                    
                                                                                                                    # 12. Create a summary of total salary paid in each 'department'.
                                                                                                                    total_salary_dept <- employees %>% 
                                                                                                                      group_by(#####) %>% 
                                                                                                                        summarize(total_salary = sum(###, na.rm = TRUE))
                                                                                                                          print(total_salary_dept)
                                                                                                                          
                                                                                                                          # 13. Filter 'employees' to only include rows where 'salary_level' == "High" and 'performance_label' == "High Performer".
                                                                                                                          top_earners <- employees %>% filter(### == "High", ### == "High Performer")
                                                                                                                            print(top_earners)
                                                                                                                            
                                                                                                                            # 14. Use distinct() to find unique combinations of 'department' and 'performance_label'.
                                                                                                                            unique_dept_perf <- employees %>% distinct(department, performance_label)
                                                                                                                            print(unique_dept_perf)
                                                                                                                            
                                                                                                                            # 15. Summarize the average 'salary_norm' by 'salary_level'.
                                                                                                                            salary_norm_summary <- employees %>%
                                                                                                                              group_by(###) %>%
                                                                                                                                summarize(avg_salary_norm = mean(###, na.rm = TRUE))
                                                                                                                                  
                                                                                                                                  print(salary_norm_summary)
                                                                                                                                  
                                                                                                                                  # 16. Create a histogram of 'salary' with binwidth of 5000.
                                                                                                                                  ggplot(employees, aes(x = ###)) +
                                                                                                                                                          geom_histogram(binwidth = ###) +
                                                                                                                                                                           labs(title = "Salary Distribution", x = "Salary", y = "Count")
                                                                                                                                                                         
                                                                                                                                                                         # 17. Select only columns 'emp_id', 'department', and 'salary' into a new data frame.
                                                                                                                                                                         basic_info <- employees %>% select(emp_id, ###, ###)
                                                                                                                                                                                                            head(basic_info)
                                                                                                                                                                                                            
                                                                                                                                                                                                            # 18. Use filter() and ifelse() in the same chain to label employees with 'Underpaid' if salary < 40000.
                                                                                                                                                                                                            employees <- employees %>%
                                                                                                                                                                                                              mutate(pay_status = ifelse(### < 40000, "Underpaid", "Fair/High"))
                                                                                                                                                                                                                
                                                                                                                                                                                                                # 19. Calculate the mean 'salary' for each 'performance_label'.
                                                                                                                                                                                                                label_salary_mean <- employees %>% group_by(performance_label) %>%
                                                                                                                                                                                                                  summarize(mean_salary = mean(###, na.rm = TRUE))
                                                                                                                                                                                                                    
                                                                                                                                                                                                                    print(label_salary_mean)
                                                                                                                                                                                                                    
                                                                                                                                                                                                                    # 20. Arrange employees by 'department' ascending, then by 'salary' descending within each department.
                                                                                                                                                                                                                    sorted_dept <- employees %>% arrange(department, desc(###))
                                                                                                                                                                                                                      head(sorted_dept)
                                                                                                                                                                                                                      
                                                                                                                                                                                                                      # 21. Create a new column 'salary_thousands' = salary / 1000.
                                                                                                                                                                                                                      employees <- employees %>% mutate(salary_thousands = ### / 1000)
                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                          # 22. Filter out employees with 'performance_score' less than 2.
                                                                                                                                                                                                                                                          employees <- employees %>% filter(performance_score >= 2)
                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                        # 23. Recode 'department' names to Title Case (e.g., 'Finance' -> 'Finance', 'IT' -> 'It') using str_to_title.
                                                                                                                                                                                                                                                        library(stringr)
                                                                                                                                                                                                                                                        employees <- employees %>% mutate(department = stringr::str_to_title(department))
                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                        # 24. Summarize the minimum and maximum salary for each 'department'.
                                                                                                                                                                                                                                                        dept_min_max <- employees %>% group_by(department) %>%
                                                                                                                                                                                                                                                          summarize(min_sal = min(salary), max_sal = max(###))
                                                                                                                                                                                                                                                            print(dept_min_max)
                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                            # 25. Create a box plot of 'salary' by 'department'.
                                                                                                                                                                                                                                                            ggplot(employees, aes(x = department, y = ###)) +
                                                                                                                                                                                                                                                                                    geom_boxplot() +
                                                                                                                                                                                                                                                                                    labs(title = "Salary by Department", x = "Department", y = "Salary")
                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                  # 26. Create a new column 'perf_binary' that is 1 if performance_score >= 3, else 0.
                                                                                                                                                                                                                                                                                  employees <- employees %>% mutate(perf_binary = ifelse(### >= 3, 1, 0))
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    # 27. Filter employees for those who are 'High Performer' OR salary > 90000.
                                                                                                                                                                                                                                                                                    amazing_or_highpaid <- employees %>% filter(performance_label == "High Performer" | ### > 90000)
                                                                                                                                                                                                                                                                                                                                  print(amazing_or_highpaid)
                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                # 28. Add a column 'department_perf' that concatenates 'department' and 'performance_label' (e.g., 'IT_High Performer').
                                                                                                                                                                                                                                                                                                                                employees <- employees %>% mutate(
                                                                                                                                                                                                                                                                                                                                  # 'department_perf' is the name of the new column we're creating.
                                                                                                                                                                                                                                                                                                                                  # 'paste()' merges text values from multiple columns, row by row.
                                                                                                                                                                                                                                                                                                                                  # 'sep = "_"' tells R to separate them with an underscore. You can also do a hypen "-" or a punctuation "."
                                                                                                                                                                                                                                                                                                                                  department_perf = paste(
                                                                                                                                                                                                                                                                                                                                    department, performance_label, sep = "_"))
                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                # 29. Calculate the proportion of each 'performance_label' within each department.
                                                                                                                                                                                                                                                                                                                                dept_perf_prop <- employees %>%
                                                                                                                                                                                                                                                                                                                                  group_by(department) %>%
                                                                                                                                                                                                                                                                                                                                  count(performance_label) %>%
                                                                                                                                                                                                                                                                                                                                  mutate(prop = n / sum(n))
                                                                                                                                                                                                                                                                                                                                print(dept_perf_prop)
                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                # 30. Use summarize() to find both min and max salary in each 'salary_level'.
                                                                                                                                                                                                                                                                                                                                salary_range <- employees %>% group_by(salary_level) %>%
                                                                                                                                                                                                                                                                                                                                  summarize(
                                                                                                                                                                                                                                                                                                                                    min_salary = min(
                                                                                                                                                                                                                                                                                                                                      ###), 
                                                                                                                                                                                                                                                                                                                                      max_salary = max(###))
                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                        print(salary_range)
                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                        # 31. Reorder 'department' factor in ascending order of median salary.
                                                                                                                                                                                                                                                                                                                                        # (Hint: fct_reorder from library(forcats))
                                                                                                                                                                                                                                                                                                                                        employees <- employees %>%
                                                                                                                                                                                                                                                                                                                                          mutate(department = forcats::fct_reorder(department, salary, .fun = median))
                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                        # 32. Create a line chart of 'performance_score' over 'emp_id' (just for demonstration, though it's not time-based).
                                                                                                                                                                                                                                                                                                                                        ggplot(employees, aes(x = emp_id, y = performance_score)) +
                                                                                                                                                                                                                                                                                                                                          geom_line() +
                                                                                                                                                                                                                                                                                                                                          labs(title = "Performance Over Employee IDs", x = "Employee ID", y = "Score")
                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                        # 33. Drop the column 'salary_thousands' if you created it.
                                                                                                                                                                                                                                                                                                                                        employees <- employees %>% select(-salary_thousands)
                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                        # 34. Create a pivot_wider or pivot_longer demonstration.
                                                                                                                                                                                                                                                                                                                                        # (Hint: pivot_longer(employees, cols = c("salary", "performance_score"), names_to = "metric", values_to = "value"))
                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                        # 35. Use sample_n() to randomly pick 10 employees.
                                                                                                                                                                                                                                                                                                                                        random_10 <- employees %>% sample_n(10)
                                                                                                                                                                                                                                                                                                                                        print(random_10)
                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                        # 36. Add a step that replaces any salary above 95000 with 95000 (a salary cap) using ifelse.
                                                                                                                                                                                                                                                                                                                                        employees <- employees %>% mutate(
                                                                                                                                                                                                                                                                                                                                          salary_capped = ifelse(
                                                                                                                                                                                                                                                                                                                                            ### > 95000, 95000, ###))
                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                            # 37. Create a new column that calculates 'perf_salary_ratio' = performance_score / salary (just a random ratio).
                                                                                                                                                                                                                                                                                                                                            employees <- employees %>% mutate(
                                                                                                                                                                                                                                                                                                                                              perf_salary_ratio = ### / ###)
                                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                # 38. Filter 'employees' for rows where 'department' is either 'HR' or 'IT'.
                                                                                                                                                                                                                                                                                                                                                hr_it_only <- employees %>% filter(department %in% c("HR", "IT"))
                                                                                                                                                                                                                                                                                                                                              print(hr_it_only)
                                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                                              # 39. Summarize to find how many 'High Performer' employees exist in each 'salary_level'.
                                                                                                                                                                                                                                                                                                                                              perf_count <- employees %>% filter(performance_label == "High Performer") %>%
                                                                                                                                                                                                                                                                                                                                                group_by(
                                                                                                                                                                                                                                                                                                                                                  ###) %>%
                                                                                                                                                                                                                                                                                                                                                  summarize(count = n())
                                                                                                                                                                                                                                                                                                                                                  print(perf_count)
                                                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                  # 40. Arrange 'employees' by 'performance_label' ascending, then by 'salary_level' descending.
                                                                                                                                                                                                                                                                                                                                                  employees <- employees %>% arrange(performance_label, desc(salary_level))
                                                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                  # 41. Replace any NA in 'salary' with the mean salary.
                                                                                                                                                                                                                                                                                                                                                  employees <- employees %>% mutate(salary = ifelse(
                                                                                                                                                                                                                                                                                                                                                    is.na(salary), mean(salary, na.rm = TRUE), salary
                                                                                                                                                                                                                                                                                                                                                  ))
                                                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                  # 42. Create a bar plot of 'salary_level' with facet_wrap by 'performance_label'.
                                                                                                                                                                                                                                                                                                                                                  ggplot(employees, aes(x = salary_level)) +
                                                                                                                                                                                                                                                                                                                                                    geom_bar() +
                                                                                                                                                                                                                                                                                                                                                    facet_wrap(~ performance_label) +
                                                                                                                                                                                                                                                                                                                                                    labs(title = "Salary Level Distribution by Performance", x = "Salary Level", y = "Count")
                                                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                  # 43. Sort the data by 'performance_label' alphabetically, then 'department' alphabetically.
                                                                                                                                                                                                                                                                                                                                                  employees <- employees %>% arrange(
                                                                                                                                                                                                                                                                                                                                                    ###, ###)
                                                                                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                                                                                    # 44. Extract only columns that contain the word "score" in their name using select().
                                                                                                                                                                                                                                                                                                                                                    employees_scores <- employees %>% select(contains("score"))
                                                                                                                                                                                                                                                                                                                                                    print(employees_scores)
                                                                                                                                                                                                                                                                                                                                                    
