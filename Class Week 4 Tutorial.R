# Session- 4
# Lexisdiagram
# Ratio 
# Lexis diagram -----------------------------------------------------------
#install and load package for producing Lexis diagrams
#install.packages("LexisPlotR")
library(LexisPlotR)


# let's say we are interested in the population aged between 20 and 30 living in Vienna between 2013 and 2023
lexis <- lexis_grid(year_start = 2013, 
                    year_end = 2025, 
                    age_start = 20, 
                    age_end = 30)


# add a life line of a student born on 2000-01-01, and move to Vienna on 2020-09-01, and leave on 2022-06-30
lexis_lifeline(lg=lexis,
               birth = "2000-01-01" , 
               entry = "2020-09-01",
               exit = "2022-06-30", 
               lineends = T, col=2, lwd=0.8)


#shade the area representing individuals age 21 to 24 
lexis_age(lg=lexis, 
          age=21, 
          delta=3)

#shade the area representing the year 2020-21
lexis_year(lg=lexis,
           year=2020,
           delta = 2)

# You can also use entry and death dates from a data.frame. It is a random dataset of 
# entry and exit dates for 300 Individuals from 1895 to 1905 that comes with the LexisPlotR package

# take a lot at the life_linessamples dataset
data("lifelines_sample") 

head(lifelines_sample)

## And let's add the lifelines to a lexis diagram
lexis2 <- lexis_grid(year_start=1895,
                     year_end=1905, 
                     age_start=0, 
                     age_end=10)

lexis2 %>%
  lexis_lifeline(birth = lifelines_sample$entry,
                 exit = lifelines_sample$exit)

# To get a better overview, let's sample 20 observations
# To make it reproductible we need to first use set.seed(###) so that others 
# can replicate our study
set.seed(2)

sample_data <- lifelines_sample %>% 
  slice_sample(n = 20) # takes a sample from the rows

lexis2 <- lexis2 %>%
  lexis_lifeline(birth = sample_data$entry,
                 exit = sample_data$exit,
                 lineends = TRUE)

# From miguels class
mylexis <- lexis_grid( year_start = 1895 ,
                       year_end = 1902 ,
                       age_start = 0 , 
                       age_end = 5)

mylexis <- lexis_year( lg = mylexis , 
                       year = 1900)


lifelines_sample <- lifelines_sample %>%
  mutate( year = year ( entry ) ) %>% 
  filter( year <1901) %>%
  group_by(year) %>%
  slice_sample(n = 10) %>% 
  ungroup ()



lexis2 <- mylexis %>%
  lexis_lifeline(birth = lifelines_sample$entry,
                 exit = lifelines_sample$exit,
                 lineends = TRUE)



mylexis_plt <- lexis_lifeline( mylexis,
                               birth = lifelines_sample$entry,
                               exit = lifelines_sample$exit, 
                               lineends = T ) +
  labs ( x = "Calendar Year" ,
         y = " Age " )




# What is the infant mortality rate (IMF) in the year 1896? ---------------------------------------
lexis2 %>% lexis_year(year=1896) %>% 
  lexis_age(age=0,
            delta=1, 
            alpha=0.1)

# Solution ----------------------------------------------------------------
# Filter the original data for births and deaths in 1896
imr_data <- lifelines_sample %>%
  filter((as.numeric(format(as.Date(entry), "%Y")) == 1896) | 
           (as.numeric(format(as.Date(exit), "%Y")) == 1896))

# Calculate the number of infant deaths (age < 1 year)
infant_deaths <- imr_data %>%
  filter(!is.na(exit) & (as.Date(exit) - as.Date(entry)) < 365) %>%
  nrow()

# Calculate the number of live births in 1899
live_births <- imr_data %>%
  filter(as.numeric(format(as.Date(entry), "%Y")) == 1896) %>%
  nrow()

# Calculate the IMR
imr <- (infant_deaths / live_births) * 1000
print(paste("Infant Mortality Rate (IMR) in 1899:", imr, "deaths per 1,000 live births"))


# what is the ASDR for age group 4-6 in the year 1902
lexis2 %>%lexis_year(year=1902) %>% 
  lexis_age(age=0,
            delta=1,
            alpha=0.1)




# What is the crude mortality rate in the year 1900? ----------------------
# a. How do we calculate the mid-year population?
# b. What is the assumptions behind it?

# Solution ----------------------------------------------------------------
# Filter the data to get deaths in the year 1900
deaths_in_1900 <- lifelines_sample %>%
  filter(!is.na(exit) & as.numeric(format(as.Date(exit), "%Y")) == 1900) %>%
  nrow()

# Step 2: Estimate mid-year population (alive during 1900)
mid_year_population <- lifelines_sample %>%
  filter((as.numeric(format(as.Date(entry), "%Y")) <= 1900) & 
           (is.na(exit) | as.numeric(format(as.Date(exit), "%Y")) >= 1900)) %>%
  nrow()

# Step 3: Calculate the Crude Mortality Rate (CMR)
cmr <- (deaths_1900 / mid_year_population) * 1000
print(paste("Crude Mortality Rate (CMR) in 1900:", round(cmr, 2), "deaths per 1,000 population"))




# Dependency ratios -------------------------------------------------------
# Let's calculate the dependency ratio for all countries in the world for all years using Wittgenstein Centre data
# Dependency ratio = (Children (0-19) + Elderly (65+)) / Working-age population (20-64)

#download the population size by age group for all countries
library(wcde)
pop <- get_wcde(indicator = "bpop") %>% 
  filter(sex=="Both") %>% 
  filter(age %in% c("0--19",
                    "20--39",
                    "40--64",
                    "65+"))
head(pop)
#Let's look at the age column 
pop %>% distinct(age)

# Age is currently a character variable. 
# Let's split it into two variables (age_start and age_end) to make it numeric

pop2 <- pop %>% 
  separate(age, into = c("age_start",
                         "age_end", 
                         sep = "--")) %>%  # It will work without "sep =" command but always do it for your future self. 
  mutate(age_start= as.numeric(age_start), 
         age_end= as.numeric(age_end))
tail(pop2)
pop2 %>% distinct(age_start)
pop2 %>% distinct(age_end)

# Let's create a new variable specifying which age groups are dependent (children and the elderly) and which age groups are not

pop3 <- pop2 %>% 
  mutate( dependency= ifelse(age_start<19|age_start>64,
                             yes="dependent",
                             no="working_age")) 

pop3 %>% distinct(dependency)

# Let's calculate the population size by country, year, and dependency status

pop4 <- pop3 %>% 
  group_by(name, year,dependency) %>% 
  summarize(population_sum=sum(bpop))

head(pop4)  

# Let's make the data wide
pop5 <- pop4 %>% 
  pivot_wider(names_from = dependency,
              values_from = population_sum)  
head(pop5)
# Finally let's calculate the dependency ratio
dep_ratio <- pop5 %>% 
  mutate(dependency_ratio=dependent/working_age)  

head(dep_ratio)

# Option 2 to calculate dependency ratio using the long data format  
dep_ratio2 <- pop4 %>%
  group_by(name, year) %>%
  summarize(dependency_ratio = population_sum[dependency=="dependent"] / population_sum[dependency == "working_age"])
head(dep_ratio2)

# Option 3 is to create the dependency indicator directly from pop
pop6 <- pop %>% mutate(dependency= ifelse(age %in%c("0--19","65+") ,
                                          yes="dependent",
                                          no="working_age"))
head(pop6)

## Aggregate, then make it wide format

pop6_01 <- pop6 %>% 
  group_by(name, year,dependency) %>% 
  summarize(population_sum=sum(bpop))  %>% 
  pivot_wider(names_from = dependency, 
              values_from = population_sum) %>% 
  ungroup()

head(pop6_01)

# As it is in wide format just calculate the dependency ratio between columns
dep_ratio3 <- pop6_01   %>% 
  mutate(dependency_ratio=dependent/working_age) 
head(dep_ratio3)


# Select five countries for visualization
selected_countries <- c("Nigeria", "China", "United States of America", "Austria" )

dep_ratio_selected <- dep_ratio %>%
  filter(name %in% selected_countries)

# Plotting the dependency ratio trends
ggplot(dep_ratio_selected, aes(x = year, y = dependency_ratio, color = name)) +
  geom_line(size = 1.2) +
  labs(title = "Dependency Ratios Over Time",
       x = "Year",
       y = "Dependency Ratio",
       color = "Country") +
  theme_minimal()


